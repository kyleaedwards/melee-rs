mod lexer;
mod token;
mod repl;
mod ast;
mod parser;
mod bytecode;
mod compiler;
mod symbols;
mod object;

use crate::repl::create_repl;

extern crate num;
#[macro_use]
extern crate num_derive;

fn main() {
    println!("Melee-rs (v0.1.0)");
    create_repl();
}
