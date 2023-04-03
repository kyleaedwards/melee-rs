use crate::lexer::Lexer;

use std::io::{self, Write};

pub fn create_repl() {
    loop {
        let mut input = String::new();
        print!("> ");
        let _ = io::stdout().flush();
        io::stdin()
            .read_line(&mut input)
            .expect("Error reading input");

        let mut lexer = Lexer::new(&input[..]);
        while let Some(token) = lexer.next() {
            println!("{:?}", token.token_type);
        }
    }
}
