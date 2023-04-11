use std::{rc::Rc, cell::RefCell, fmt};

use crate::{bytecode::Bytecode, vm::{VM, ExecutionError}};

/// Call "stack" frame (might not be in the call stack) representing
/// a function's execution context.
///
#[derive(Debug)]
pub struct Frame {
    pub ip: i32,
    pub base: i32,
    pub closure: Closure
}

impl Frame {
    pub fn new(closure: Closure, base: i32) -> Frame {
        Frame{
            ip: -1,
            closure,
            base
        }
    }

    pub fn instructions(&self) -> Option<&Bytecode> {
        match &self.closure.callable {
            Callable::Fn { instructions, .. } => Some(&instructions),
            Callable::Gen { instructions, .. } => Some(&instructions),
            _ => None
        }
    }
}

/// Collection of frame and stack information representing
/// the current state of execution.
///
#[derive(Debug)]
pub struct ExecutionState {
    pub stack: Rc<RefCell<Vec<Rc<Object>>>>,
    pub sp: i32,
    pub frames: Rc<RefCell<Vec<Frame>>>,
    pub fp: i32,
    pub parent: Option<Rc<RefCell<ExecutionState>>>,
    pub seq: Option<Iterable>
}

/// MIDI value object interface for use in Melee runtimes.
///
pub struct MidiValue {
  pub channel: u8,
  pub kind: String,
  pub data: Vec<i32>
}

trait Midi {
    fn get_value(&self) -> MidiValue;
}

#[derive(Debug)]
pub struct MidiNote {
    pub channel: u8,
    pub pitch: i32,
    pub duration: i32,
    pub velocity: i32
}

impl Midi for MidiNote {
    fn get_value(&self) -> MidiValue {
        MidiValue{
            channel: self.channel,
            kind: String::from("note"),
            data: vec![self.pitch, self.duration, self.velocity]
        }
    }
}

#[derive(Debug)]
pub struct MidiCc {
    pub channel: u8,
    pub key: i32,
    pub value: i32
}

impl Midi for MidiCc {
    fn get_value(&self) -> MidiValue {
        MidiValue{
            channel: self.channel,
            kind: String::from("cc"),
            data: vec![self.key, self.value]
        }
    }
}

#[derive(Debug)]
pub struct MidiHold {
    pub channel: u8,
    pub pitch: i32,
    pub duration: i32
}

impl Midi for MidiHold {
    fn get_value(&self) -> MidiValue {
        MidiValue{
            channel: self.channel,
            kind: String::from("hold"),
            data: vec![self.pitch, self.duration]
        }
    }
}

#[derive(Debug)]
struct MidiRest {
    pub channel: u8,
    pub duration: i32
}

impl Midi for MidiRest {
    fn get_value(&self) -> MidiValue {
        MidiValue{
            channel: self.channel,
            kind: String::from("rest"),
            data: vec![self.duration]
        }
    }
}

trait Inspectable {
    fn inspect() -> String;
}

pub enum Callable {
    VirtualFn(fn(Vec<Box<Object>>) -> Box<Object>),
    Fn{
        instructions: Bytecode,
        repr: String,
        num_locals: usize,
        num_params: usize
    },
    Gen{
        instructions: Bytecode,
        repr: String,
        num_locals: usize,
        num_params: usize
    },
    NativeFn{
        label: String,
        func: fn(&VM, Vec<Box<Object>>) -> Result<Box<Object>, ExecutionError>
    }
}

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fn { repr, .. } => write!(f, "{}", repr),
            Self::Gen { repr, .. } => write!(f, "{}", repr),
            _ => write!(f, "<native fn>")
        }
    }
}

#[derive(Debug)]
pub struct Closure {
    pub callable: Callable,
    pub vars: Vec<Box<Object>>,
}

#[derive(Debug)]
pub enum Iterable {
    Seq{
        generator: Closure,
        execution_state: Rc<RefCell<ExecutionState>>
    },
    VirtualSeq{
        next: fn() -> Object
    }
}

#[derive(Debug)]
pub enum Object {
  Null,
  Err(String),
  Return(Box<Object>),
  Yield(Box<Object>),
  Int(i32),
  Bool(bool),
  Arr(Vec<Box<Object>>),
  Callable(Callable),
  Iterable(Iterable),
  Closure(Closure),
  Note(MidiNote),
  Cc(MidiCc),
  Hold(MidiHold),
  Rest(MidiRest),
}
