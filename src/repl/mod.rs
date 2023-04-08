use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::compiler::Compiler;

use std::io::{self, Write};

pub fn create_repl() {
    loop {
        let mut input = String::new();
        print!("> ");
        let _ = io::stdout().flush();
        io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");


        // Debug tokens
        let mut lexer = Lexer::new(&input[..]);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            for err in parser.errors.iter() {
                println!("[ERROR]: {}", err);
            }
        } else {
            println!("{:?}", program);
        }

        let mut compiler = Compiler::new(Vec::new(), None);
        if let Err(compilation_error) = compiler.compile_program(program) {
            println!("{}", compilation_error);
        } else {
            println!("All good!");
            println!("{:?}", compiler.get_instructions());
        }
    }
}
