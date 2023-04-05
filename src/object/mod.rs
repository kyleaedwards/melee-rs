use std::{rc::Rc, cell::RefCell};

use crate::bytecode::Bytecode;

/// Call "stack" frame (might not be in the call stack) representing
/// a function's execution context.
///
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
pub struct ExecutionState {
    stack: Vec<Option<Object>>,
    sp: i32,
    frames: Vec<Frame>,
    fp: i32,
    parent: Option<Rc<RefCell<ExecutionState>>>,
    seq: Option<Iterable>
}

/// MIDI value object interface for use in Melee runtimes.
///
struct MidiValue {
  channel: u8,
  kind: String,
  data: Vec<i32>
}

trait Midi {
    fn get_value(&self) -> MidiValue;
}

struct MidiNote {
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

struct MidiCc {
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

struct MidiHold {
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

struct VM {}

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
        func: fn(&VM, Vec<Box<Object>>) -> Box<Object>
    }
}

pub struct Closure {
    callable: Callable,
    vars: Vec<Box<Object>>,
}

pub enum Iterable {
    Seq{
        generator: Closure,
        execution_state: Rc<RefCell<ExecutionState>>
    },
    VirtualSeq{
        next: fn() -> Object
    }
}

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
