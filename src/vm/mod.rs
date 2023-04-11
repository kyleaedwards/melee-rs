use std::{fmt::{self, Display}, collections::HashMap, rc::Rc, cell::RefCell};

use crate::{object::{Object, Frame, Callable, ExecutionState, Closure}, compiler::Compiler, bytecode::unpack_big_endian};

const MAX_FRAME_SIZE: usize = 1024;
const MAX_STACK_SIZE: usize = 1024;
const MAX_VARIABLES: usize = 65536;

pub struct ExecutionError(pub String);

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ExecutionError(msg) = self;
        write!(f, "Execution error: {}", msg)
    }
}

type VMCallbackFn = fn(Vec<Box<Object>>);

pub struct VM{
    constants: Vec<Box<Object>>,
    variables: Vec<Box<Object>>,
    stack: Rc<RefCell<Vec<Rc<Object>>>>,
    sp: i32,
    frames: Rc<RefCell<Vec<Frame>>>,
    fp: i32,
    coroutine: Option<Rc<RefCell<ExecutionState>>>,
    callbacks: HashMap<String, VMCallbackFn>
}

/// Virtual stack machine for executing instructions.
///
impl VM {
    pub fn new(compiler: &mut Compiler, variables: Option<Vec<Box<Object>>>) -> VM {
        let instructions = compiler.get_instructions();
        let constants = compiler.get_constants();
        let mut vm = VM{
            constants,
            variables: Vec::new(),
            stack: Rc::new(RefCell::new(Vec::with_capacity(MAX_STACK_SIZE))),
            sp: 0,
            frames: Rc::new(RefCell::new(Vec::with_capacity(MAX_FRAME_SIZE))),
            fp: 1,
            coroutine: None,
            callbacks: HashMap::new()
        };
        let closure = Closure{
            callable: Callable::Fn{
                instructions,
                repr: String::from("<MAIN>"),
                num_locals: 0,
                num_params: 0
            },
            vars: Vec::new(),
        };
        vm.frames.borrow_mut().push(Frame::new(closure, 0));
        vm
    }

    /// Create a new coroutine execution state for a generator sequence.
    ///
    fn create_coroutine(&mut self, closure: Closure, args: Vec<Rc<Object>>, num_locals: i32) -> ExecutionState {
        let parent = self.coroutine.as_mut().unwrap().clone();
        let parent_execution_state = ExecutionState{
            stack: self.stack.clone(),
            sp: self.sp,
            frames: self.frames.clone(),
            fp: self.fp,
            parent: Some(parent),
            seq: None
        };
        let mut frames = Vec::with_capacity(MAX_FRAME_SIZE);
        frames.push(Frame::new(closure, 0));
        let mut stack = Vec::with_capacity(MAX_STACK_SIZE);
        let sp = num_locals;
        for arg in args {
            stack.push(arg.clone());
        }
        ExecutionState{
            stack: Rc::new(RefCell::new(stack)),
            sp,
            frames: Rc::new(RefCell::new(frames)),
            fp: 1,
            parent: Some(Rc::new(RefCell::new(parent_execution_state))),
            seq: None
        }
  }

    /// Enters a new coroutine by replacing the VM execution state with one saved
    /// by a generator sequence.
    ///
    fn enter_coroutine(&mut self, mut execution_state: ExecutionState) {
        if !self.coroutine.is_none() {
            let coroutine = self.coroutine.take().unwrap().clone();
            coroutine.borrow_mut().stack = self.stack.clone();
            coroutine.borrow_mut().sp = self.sp;
            coroutine.borrow_mut().frames = self.frames.clone();
            coroutine.borrow_mut().fp = self.fp;
            execution_state.parent = Some(coroutine);
        }
        self.stack = execution_state.stack.clone();
        self.sp = execution_state.sp;
        self.frames = execution_state.frames.clone();
        self.fp = execution_state.fp;
        self.coroutine = Some(Rc::new(RefCell::new(execution_state)));
    }

    /// Leaves the current coroutine context and restore the old
    /// VM execution state.
    ///
    fn leave_coroutine(&mut self) -> Result<(), ExecutionError> {
        let (execution_state, has_parent) = {
            if let Some(exec_state) = &self.coroutine {
                let has_parent = exec_state.borrow().parent.is_some();
                (Some(exec_state), has_parent)
            } else {
                (None, false)
            }
        };
        if !has_parent {
            return Err(ExecutionError(String::from("Cannot leave root execution state")));
        }

        let execution_state = self.coroutine.as_ref().unwrap().clone();
        let mut execution_state = execution_state.borrow_mut();
        execution_state.stack = self.stack.clone();
        execution_state.sp = self.sp;
        execution_state.frames = self.frames.clone();
        execution_state.fp = self.fp;

        self.coroutine = if execution_state.parent.is_some() {
            Some(execution_state.parent.as_ref().unwrap().clone())
        } else {
            None
        };

        let parent = execution_state.parent.as_ref().unwrap().borrow();
        self.stack = parent.stack.clone();
        self.sp = parent.sp;
        self.frames = parent.frames.clone();
        self.fp = parent.fp;

        Ok(())
    }

    /// Returns the last object popped off the top of the stack, or
    /// undefined if the stack is empty.
    ///
    pub fn last_element(&self) -> Option<Rc<Object>> {
        let stack = self.stack.borrow();
        let elem = stack.last();
        if let Some(elem) = elem {
            Some(elem.clone())
        } else {
            None
        }
    }

    /// Pushes a new object onto the VM stack and increments
    /// the stack pointer.
    ///
    fn push(&mut self, o: Object) -> Result<(), ExecutionError> {
        if self.sp as usize >= MAX_STACK_SIZE {
            return Err(ExecutionError(String::from("Maximum stack size exceeded")))
        }
        let mut stack = self.stack.borrow_mut();
        stack.push(Rc::new(o));
        self.sp += 1;
        Ok(())
    }

    /// Pops a new object off the VM stack and decrements
    /// the stack pointer.
    ///
    fn pop(&mut self) -> Option<Rc<Object>> {
        if self.sp <= 0 {
            None
        } else {
            self.sp -= 1;
            let stack = self.stack.borrow();
            Some(stack[self.sp as usize].clone())
        }
    }

    /// Jumps to next instruction specified by the next two instruction
    /// bytes.
    ///
    fn jump(&mut self) {
        let mut frames = self.frames.borrow_mut();
        let len = frames.len();
        let instructions = frames[len - 1].instructions().unwrap();
        let destination = unpack_big_endian(instructions, frames[len - 1].ip as u32 + 1, 2);
        frames[len - 1].ip = destination as i32 - 1;
    }

    /// Reads operand at offset.
    ///
    fn read_operand(&mut self, width: i32) -> i32 {
        let mut frames = self.frames.borrow_mut();
        let len = frames.len();
        let instructions = frames[len - 1].instructions().unwrap();
        let destination = unpack_big_endian(instructions, frames[len - 1].ip as u32 + 1, 2);
        frames[len - 1].ip += width;
        return destination as i32;
    }
}

impl Display for VM {
    /// Pretty-prints information about the VM state.
    ///
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut curr = self.sp;
        write!(f, "SP {}\n", curr)?;

        let frames = self.frames.borrow();
        let closure = &frames.last().unwrap().closure;
        write!(f, "FRAME {:?}\n", closure)?;
        write!(f, "\nCVARS\n")?;
        for var in &closure.vars {
            write!(f, "  {:?}\n", var)?;
        }
        write!(f, "\nCONSTS\n")?;
        for c in &self.constants {
            write!(f, "  {:?}\n", c)?;
        }
        while curr >= 0 {
            let item = &self.stack.borrow()[curr as usize];
            curr -= 1;
            write!(f, "{:0>4} {:?}\n", curr, item)?;
        }
        Ok(())
    }
}