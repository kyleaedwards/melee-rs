use crate::{object::{Object, Callable}, vm::{VM, ExecutionError}};


type NFN = fn(&VM, Vec<Box<Object>>) -> Box<Object>;

fn handle_channel(_vm: &VM, args: Vec<Box<Object>>) -> Result<Box<Object>, ExecutionError> {
    if args.len() == 1 {
        match args[0].as_ref() {
            Object::Note(note) => {
                return Ok(Box::new(Object::Int(note.channel as i32)))
            }
            Object::Cc(cc) => {
                return Ok(Box::new(Object::Int(cc.channel as i32)))
            }
            _ => {}
        }
    }
    Err(ExecutionError(String::from("Function `chan` takes a single MIDI note or CC argument")))
}

pub const NATIVE_FNS: [&str; 1] = [
    "chan"
];

pub fn get_native_fn(key: &str) -> Option<Box<Object>> {
    let label = String::from(key);
    match key {
        /**
         * chan(Note): Int
         * Given a MIDI note or CC object, returns its channel.
         */
        "chan" => Some(Box::new(Object::Callable(Callable::NativeFn{
            label,
            func: handle_channel
        }))),
        _ => None
    }
}

