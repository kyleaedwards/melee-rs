use std::{fmt::{self, Display}, collections::HashMap, rc::Rc, cell::RefCell};

use crate::{object::{Object, Frame, Callable, ExecutionState, Closure, Iterable}, compiler::Compiler, bytecode::{unpack_big_endian, OPCODES, Opcode}, builtins::{NATIVE_FNS, get_native_fn}};

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

/// The Melee virtual machine 
///
pub struct VM{
    constants: Vec<Rc<Object>>,
    variables: Vec<Rc<Object>>,
    stack: Rc<RefCell<Vec<Rc<Object>>>>,
    sp: i32,
    frames: Rc<RefCell<Vec<Rc<RefCell<Frame>>>>>,
    fp: i32,
    coroutine: Option<Rc<RefCell<ExecutionState>>>,
    callbacks: HashMap<String, VMCallbackFn>
}

/// Virtual stack machine for executing instructions.
///
impl VM {
    pub fn new(compiler: &mut Compiler, variables: Option<Vec<Rc<Object>>>) -> VM {
        let instructions = compiler.get_instructions();
        let constants = compiler.get_constants();
        let variables = if let Some(vars) = variables {
            vars
        } else {
            Vec::new()
        };
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
        let closure = Rc::new(RefCell::new(Closure{
            callable: Rc::new(Callable::Fn{
                instructions: Rc::new(instructions),
                repr: String::from("<MAIN>"),
                num_locals: 0,
                num_params: 0
            }),
            vars: Vec::new(),
        }));
        vm.frames.borrow_mut().push(Rc::new(RefCell::new(Frame::new(closure, 0))));
        vm
    }

    /// Create a new coroutine execution state for a generator sequence.
    ///
    fn create_coroutine(&mut self, closure: Rc<RefCell<Closure>>, args: Vec<Rc<Object>>, num_locals: i32) -> ExecutionState {
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
        frames.push(Rc::new(RefCell::new(Frame::new(closure, 0))));
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
    fn enter_coroutine(&mut self, mut execution_state: Rc<RefCell<ExecutionState>>) {
        if !self.coroutine.is_none() {
            let coroutine = self.coroutine.take().unwrap().clone();
            coroutine.borrow_mut().stack = self.stack.clone();
            coroutine.borrow_mut().sp = self.sp;
            coroutine.borrow_mut().frames = self.frames.clone();
            coroutine.borrow_mut().fp = self.fp;
            execution_state.borrow_mut().parent = Some(coroutine);
        }
        let exec = execution_state.borrow();
        self.stack = exec.stack.clone();
        self.sp = exec.sp;
        self.frames = exec.frames.clone();
        self.fp = exec.fp;
        self.coroutine = Some(execution_state.clone());
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
    fn push(&mut self, o: Rc<Object>) -> Result<(), ExecutionError> {
        if self.sp as usize >= MAX_STACK_SIZE {
            return Err(ExecutionError(String::from("Maximum stack size exceeded")))
        }
        let mut stack = self.stack.borrow_mut();
        stack.push(o);
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
        let frames = self.frames.borrow_mut();
        let len = frames.len();
        let f = frames[len - 1].borrow();
        let instructions = f.instructions().unwrap();
        let destination = unpack_big_endian(&instructions, frames[len - 1].borrow().ip as u32 + 1, 2);
        frames[len - 1].borrow_mut().ip = destination as i32 - 1;
    }

    /// Reads operand at offset.
    ///
    fn read_operand(&self, width: i32) -> i32 {
        let frames = self.frames.borrow_mut();
        let len = frames.len();
        let f = frames[len - 1].borrow();
        let instructions = f.instructions().unwrap();
        let destination = unpack_big_endian(&instructions, frames[len - 1].borrow().ip as u32 + 1, 2);
        frames[len - 1].borrow_mut().ip += width;
        return destination as i32;
    }

    /// Calculate a sequence's next value and retrieve the value from the stack.
    ///
    pub fn take_next(&mut self, seq: Box<Object>) -> Result<Rc<Object>, ExecutionError> {
        match *seq {
            Object::Iterable(iter) => {
                match iter {
                    Iterable::VirtualSeq { next, .. } => {
                        let obj = next()?;
                        Ok(Rc::new(obj))
                    }
                    _ => {
                        let exit_frame = self.next(iter)?;
                        self.run(exit_frame);
                        if let Some(elem) = self.pop() {
                            Ok(elem)
                        } else {
                            Err(ExecutionError(String::from("Expecting valid output from sequence")))
                        }
                    }
                }
            },
            _ => Err(ExecutionError(String::from("Cannot call next on a non-sequence")))
        }
    }

    pub fn next(&mut self, seq: Iterable) -> Result<Option<Rc<RefCell<Frame>>>, ExecutionError> {
        match seq {
            Iterable::Seq { done, generator, execution_state } => {
                if done {
                    self.push(Rc::new(Object::Null))?;
                    return Ok(None);
                }
                let frames = self.frames.clone();
                let len = frames.borrow().len();
                let frame = frames.borrow()[len - 1].clone();
                self.enter_coroutine(execution_state.clone());
                Ok(Some(frame))
            },
            _ => Err(ExecutionError(String::from("`next` can only be used on generated sequence instances")))
        }
    }

    /// Iterates over the compiler instructions item-by-item, using the
    /// stack to hold values and perform operations.
    ///
    /// @param exit_frame - Frame on which to halt execution
    ///
    pub fn run(&mut self, exit_frame: Option<Rc<RefCell<Frame>>>) -> Result<(), ExecutionError> {
        let frame = {
            let frames = self.frames.borrow();
            let len = frames.len();
            frames[len - 1].clone()
        };
        let f = frame.borrow();
        let instructions = f.instructions();
        let mut inst = instructions.as_ref();

        while inst.is_some() && frame.borrow().ip <= inst.unwrap().len() as i32 {
            // The VM can be run recursively, but in doing so, you must
            // specify an exit frame in which to bounce out. This is
            // particularly useful because the next item on the stack
            // is the return value from the exited frame.
            if is_exit_frame(&frame, &exit_frame) {
                break;
            }

            let mut frame_borrow = frame.borrow_mut();
            let instructions = inst.unwrap();
            frame_borrow.ip += 1;
            let ip = frame_borrow.ip;
            let op = instructions[ip as usize];
            let opcode = OPCODES[op as usize].as_ref().unwrap().opcode;

            match opcode {
                Opcode::Const => {
                    let idx = self.read_operand(2) as usize;
                    let cnst = self.constants[idx].clone();
                    self.push(cnst)?;
                },
                Opcode::Closure => {
                    let idx = self.read_operand(2) as usize;
                    let num_free = self.read_operand(1) as usize;
                    let func = self.constants[idx].clone();
                    let callable = if let Object::Callable(callable) = func.as_ref() {
                        callable.clone()
                    } else {
                        return Err(ExecutionError(String::from("Invalid closure function")));
                    };
                    let mut closure_vars = Vec::with_capacity(num_free);
                    for i in 0..num_free {
                        let item_idx = (self.sp as usize) - num_free + i;
                        let item = {
                            let stack = self.stack.borrow();
                            stack[item_idx].clone()
                        };
                        closure_vars.push(item);
                    }
                    self.sp -= num_free as i32;
                    self.push(Rc::new(Object::Closure(Closure { callable, vars: closure_vars })))?;
                },
                Opcode::SelfClosure => {
                    self.push(Rc::new(Object::RefClosure(f.closure.clone())))?;
                },
                Opcode::Array => {
                    let size = self.read_operand(2) as usize;
                    let mut arr = Vec::with_capacity(size);
                    let start = (self.sp as usize) - size;
                    for i in 0..size {
                        let el = self.stack.borrow()[start + i].clone();
                        arr.push(el);
                    }
                    self.sp -= size as i32;
                    self.push(Rc::new(Object::Arr(arr)))?;
                },
                Opcode::Len => {
                    let arr = self.pop();
                    if let Some(arr) = arr {
                        let a = arr.clone();
                        if let Object::Arr(arr) = a.as_ref() {
                            self.push(Rc::new(Object::Int(arr.len() as i32)))?;
                        }
                    } else {
                        return Err(ExecutionError(String::from("Cannot retrieve length of non-array")));
                    }
                },
                Opcode::Index => {
                    let idx = if let Some(idx) = self.pop() {
                        idx
                    } else {
                        return Err(ExecutionError(String::from("Array index must be an integer")));
                    };
                    let idx = if let Object::Int(i) = idx.as_ref() {
                        *i as usize
                    } else {
                        return Err(ExecutionError(String::from("Array index must be an integer")));
                    };
                    let collection = if let Some(collection) = self.pop() {
                        collection
                    } else {
                        return Err(ExecutionError(String::from("Cannot index into a non-array")));
                    };
                    let collection = if let Object::Arr(collection) = collection.as_ref() {
                        collection
                    } else {
                        return Err(ExecutionError(String::from("Array index must be an integer")));
                    };
                    if idx >= collection.len() {
                        self.push(Rc::new(Object::Null))?;
                    } else {
                        self.push(collection[idx].clone())?;
                    }
                }
                Opcode::Pop => {
                    self.pop();
                },
                Opcode::True => {
                    self.push(Rc::new(Object::Bool(true)))?;
                },
                Opcode::False => {
                    self.push(Rc::new(Object::Bool(false)))?;
                },
                Opcode::Null => {
                    self.push(Rc::new(Object::Null))?;
                },
                Opcode::SetGlobal => {
                    let idx = self.read_operand(2) as usize;
                    self.variables[idx] = if let Some(val) = self.pop() {
                        val.clone()
                    } else {
                        Rc::new(Object::Null)
                    };
                },
                Opcode::GetGlobal => {
                    let idx = self.read_operand(2) as usize;
                    let val = self.variables[idx].clone();
                    self.push(val)?;
                },
                Opcode::Set => {
                    let base = frame_borrow.base as usize;
                    let idx = self.read_operand(1) as usize;
                    let obj = if let Some(val) = self.pop() {
                        val.clone()
                    } else {
                        Rc::new(Object::Null)
                    };
                    let mut stack = self.stack.borrow_mut();
                    let stack = stack.as_mut_slice();
                    stack[base + idx] = obj;
                },
                Opcode::Get => {
                    let idx = self.read_operand(1) as usize;
                    let base = frame_borrow.base as usize;
                    let val = self.stack.borrow()[base + idx].clone();
                    self.push(val)?;
                },
                Opcode::SetClosure => {
                    let idx = self.read_operand(1) as usize;
                    let obj = if let Some(val) = self.pop() {
                        val.clone()
                    } else {
                        Rc::new(Object::Null)
                    };
                    let closure = frame_borrow.closure.clone();
                    let mut closure = closure.borrow_mut();
                    closure.vars[idx] = obj;
                },
                Opcode::GetClosure => {
                    let idx = self.read_operand(1) as usize;
                    let closure = frame_borrow.closure.clone();
                    let val = closure.borrow().vars[idx].clone();
                    self.push(val)?;
                },
                Opcode::GetNative => {
                    let idx = self.read_operand(1) as usize;
                    let func = NATIVE_FNS[idx];
                    self.push(get_native_fn(func).unwrap())?;
                },
                Opcode::Bang => {
                    let obj = if let Some(val) = self.pop() {
                        val.clone()
                    } else {
                        Rc::new(Object::Null)
                    };
                    let obj = obj.as_ref();
                    match obj {
                        Object::Int(i) => {
                            self.push(Rc::new(Object::Bool(*i == 0)))?;
                        },
                        Object::Bool(b) => {
                            self.push(Rc::new(Object::Bool(!*b)))?;
                        },
                        Object::Null => {
                            self.push(Rc::new(Object::Bool(true)))?;
                        },
                        _ => {
                            return Err(ExecutionError(String::from("Bang operator not supported on this data type")));
                        }
                    }
                },
                Opcode::Minus => {
                    let obj = if let Some(val) = self.pop() {
                        val.clone()
                    } else {
                        Rc::new(Object::Null)
                    };
                    let obj = obj.as_ref();
                    match obj {
                        Object::Int(i) => {
                            self.push(Rc::new(Object::Int(-i)))?;
                        },
                        _ => {
                            return Err(ExecutionError(String::from("Minus operator not supported on this data type")));
                        }
                    }
                },
                Opcode::Add => {
                    let val = self.infix_operation(opcode)?;
                    self.push(Rc::new(Object::Int(val)))?;
                },
                Opcode::Subtract => {
                    let val = self.infix_operation(opcode)?;
                    self.push(Rc::new(Object::Int(val)))?;
                },
                Opcode::Multiply => {
                    let val = self.infix_operation(opcode)?;
                    self.push(Rc::new(Object::Int(val)))?;
                },
                Opcode::Divide => {
                    let val = self.infix_operation(opcode)?;
                    self.push(Rc::new(Object::Int(val)))?;
                },
                Opcode::Modulus => {
                    let val = self.infix_operation(opcode)?;
                    self.push(Rc::new(Object::Int(val)))?;
                },
                Opcode::Equals => {
                    let val = self.comparison_operation(opcode)?;
                    self.push(Rc::new(Object::Bool(val)))?;
                },
                Opcode::NotEquals => {
                    let val = self.comparison_operation(opcode)?;
                    self.push(Rc::new(Object::Bool(val)))?;
                },
                Opcode::GreaterThan => {
                    let val = self.comparison_operation(opcode)?;
                    self.push(Rc::new(Object::Bool(val)))?;
                },
                Opcode::GreaterThanEquals => {
                    let val = self.comparison_operation(opcode)?;
                    self.push(Rc::new(Object::Bool(val)))?;
                },
                Opcode::And => {
                    let val = {
                        let stack = self.stack.borrow();
                        let stack = stack.as_slice();
                        let left = &stack[self.sp as usize - 2];
                        let right = &stack[self.sp as usize - 1];
                        self.sp -= 2;
                        left.is_truthy() && right.is_truthy()
                    };
                    self.push(Rc::new(Object::Bool(val)))?;
                },
                Opcode::Or => {
                    let val = {
                        let stack = self.stack.borrow();
                        let stack = stack.as_slice();
                        let left = &stack[self.sp as usize - 2];
                        let right = &stack[self.sp as usize - 1];
                        self.sp -= 2;
                        left.is_truthy() || right.is_truthy()
                    };
                    self.push(Rc::new(Object::Bool(val)))?;
                },
                Opcode::Jump => self.jump(),
                Opcode::JumpIfNot => {
                    let obj = if let Some(val) = self.pop() {
                        val.clone()
                    } else {
                        Rc::new(Object::Null)
                    };
                    if !obj.is_truthy() {
                        self.jump();
                    } else {
                        let frames = self.frames.borrow_mut();
                        let len = frames.len();
                        frames[len - 1].borrow_mut().ip += 2;
                    }
                },
                Opcode::Call => {
                    let idx = self.read_operand(1) as usize;
                }
                _ => ()
            };
        }
        Ok(())
    }

    fn infix_operation(&mut self, op: Opcode) -> Result<i32, ExecutionError> {
        let mut stack = self.stack.borrow_mut();
        let stack = stack.as_mut_slice();
        let left = &stack[self.sp as usize - 2];
        let right = &stack[self.sp as usize - 1];
        self.sp -= 2;
        let left = match left.as_ref() {
            Object::Int(i) => {
                *i
            },
            _ => {
                return Err(ExecutionError(String::from("Math infix operator not supported on this data type")));
            }
        };
        let right = match right.as_ref() {
            Object::Int(i) => {
                *i
            },
            _ => {
                return Err(ExecutionError(String::from("Math infix operator not supported on this data type")));
            }
        };

        let val = match op {
            Opcode::Add => left + right,
            Opcode::Subtract => left - right,
            Opcode::Multiply => left * right,
            Opcode::Divide => left / right,
            Opcode::Modulus => left % right,
            _ => return Err(ExecutionError(String::from("Unhandled binary integer operator")))
        };

        Ok(val)
    }

    fn comparison_operation(&mut self, op: Opcode) -> Result<bool, ExecutionError> {
        let mut stack = self.stack.borrow_mut();
        let stack = stack.as_mut_slice();
        let left = &stack[self.sp as usize - 2];
        let right = &stack[self.sp as usize - 1];
        self.sp -= 2;
        let res = match left.as_ref() {
            Object::Int(i) => {
                if let Object::Int(j) = right.as_ref() {
                    match op {
                        Opcode::Equals => *i == *j,
                        Opcode::NotEquals => *i != *j,
                        Opcode::GreaterThan => *i > *j,
                        Opcode::GreaterThanEquals => *i >= *j,
                        _ => {
                            return Err(ExecutionError(String::from("Unrecognized comparison operator")));
                        }
                    }
                } else {
                    return Err(ExecutionError(String::from("Must compare two variables of the same type")));
                }
            },
            Object::Bool(i) => {
                if let Object::Bool(j) = right.as_ref() {
                    match op {
                        Opcode::Equals => *i == *j,
                        Opcode::NotEquals => *i != *j,
                        Opcode::GreaterThan => *i > *j,
                        Opcode::GreaterThanEquals => *i >= *j,
                        _ => {
                            return Err(ExecutionError(String::from("Unrecognized comparison operator")));
                        }
                    }
                } else {
                    return Err(ExecutionError(String::from("Must compare two variables of the same type")));
                }
            },
            _ => {
                return Err(ExecutionError(String::from("Comparison infix operator not supported on this data type")));
            }
        };
        Ok(res)
    }

    fn call(&mut self, callee: Rc<Object>, mut num_args: usize) -> Result<Option<Rc<RefCell<Frame>>>, ExecutionError> {
        match callee.as_ref() {
            Object::Closure(closure) => {
                match closure.callable.as_ref() {
                    Callable::Fn { instructions, repr, num_locals, num_params } => {
                        while num_args > *num_params {
                            self.pop();
                            num_args -= 1;
                        }
                        while num_args < *num_params {
                            self.push(Rc::new(Object::Null))?;
                            num_args += 1;
                        }
                        let frame = Frame::new(callee.clone(), self.sp - (num_args as i32));
                        self.frames.borrow_mut()[self.fp as usize] = Rc::new(RefCell::new(frame));
                        self.fp += 1;
                        self.sp = frame.base + (*num_locals as i32);
                
                        // Specify an exit frame.
                        return Ok(Some(self.frames.borrow()[self.fp as usize - 2].clone()));
                    },
                    Callable::Gen { instructions, repr, num_locals, num_params } => {
                        while num_args > *num_params {
                            self.pop();
                            num_args -= 1;
                        }
                        while num_args < *num_params {
                            self.push(Rc::new(Object::Null))?;
                            num_args += 1;
                        }
                        let args = self.gather_args(num_args);
                        let execution_state = self.create_coroutine(callee.clone(), args, num_locals);
                        self.push(
                            Rc::new(Object::Iterable(
                                Iterable::Seq {
                                    done: false,
                                    generator: callee.clone(),
                                    execution_state: Rc::new(RefCell::new(execution_state))
                                }
                            ),
                          );
                    },
                    _ => {
                        return Err(ExecutionError(String::from("Unexpected call")));
                    }
                }
            },
            Object::Callable(callable) => {
                match callable.as_ref() {
                    Callable::NativeFn { label, func } => {
                        let args = self.gather_args(num_args);
                        let res = func(self, args)?;
                        self.push(res)?;
                    },
                    _ => {
                        return Err(ExecutionError(String::from("Callable was not properly wrapped in a closure")));
                    }
                }
                None
            },
            _ => {
                return Err(ExecutionError(String::from("Cannot perform opcode CALL on a non-callable stack element")));
            }
        }

          if (fn instanceof obj.Fn) {
            const frame = new obj.Frame(callee, this.sp - numArgs);
            this.frames[this.fp] = frame;
            this.fp++;
            this.sp = frame.base + fn.numLocals;
    
            // Specify an exit frame.
            return this.frames[this.fp - 2];
          } else if (fn instanceof obj.Gen) {
            const args = this.gatherArgs(numArgs);
            this.push(
              new obj.Seq(
                callee,
                this.createCoroutine(callee, args, fn.numLocals),
              ),
            );
          }
      }
}

fn is_exit_frame(frame: &Rc<RefCell<Frame>>, exit_frame: &Option<Rc<RefCell<Frame>>>) -> bool {
    if let Some(exit_frame) = exit_frame {
        Rc::ptr_eq(frame, exit_frame)
    } else {
        false
    }
}

impl Display for VM {
    /// Pretty-prints information about the VM state.
    ///
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut curr = self.sp;
        write!(f, "SP {}\n", curr)?;

        let frames = self.frames.borrow();
        let closure = &frames.last().unwrap().borrow().closure;
        let closure = closure.borrow();
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