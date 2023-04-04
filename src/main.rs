mod lexer;
mod token;
mod repl;
mod ast;
mod parser;

use crate::repl::create_repl;

fn main() {
    println!("Melee-rs (v0.1.0)");
    create_repl();
}
