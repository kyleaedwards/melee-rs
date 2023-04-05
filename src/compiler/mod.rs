use std::cell::RefCell;
use std::rc::Rc;

use crate::bytecode::{Bytecode, Opcode};
use crate::object::Object;
use crate::symbols::{SymbolTable, ScopeType};

struct CompiledInstruction {
    pub opcode: Opcode,
    pub position: i32,
}

struct CompilerScope {
    /// Serial bytecode instructions representing a program or function body.
    ///
    pub instructions: Bytecode,

    /// Saved instruction to backtrack or remove previous items from the bytecode.
    /// This is primarily used to support implicit returns from block statements.
    ///
    pub last_instruction: CompiledInstruction,

    /// Saved instruction to backtrack or remove previous items from the bytecode.
    /// This is primarily used to support implicit returns from block statements.
    ///
    pub previous_instruction: CompiledInstruction
}

struct Compiler {
    scopes: Vec<CompilerScope>,
    scope_index: i32,
    symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Object>,
    loopStarts: Vec<usize>,
    breaks: Vec<Vec<usize>>
}

impl Compiler {
    pub fn new(constants: Vec<Object>, symbol_table: Rc<RefCell<SymbolTable>>) -> Compiler {
        let mut base_symbol_table = SymbolTable::new(ScopeType::Native, None);
        // for func in native_functions {
        //     base_symbol_table.add(func.label);
        // }

        let mut compiler = Compiler{
            scopes: Vec::new(),
            scope_index: -1,
            symbol_table: Rc::new(RefCell::new(base_symbol_table)),
            constants,
            loopStarts: Vec::new(),
            breaks: Vec::new()
        };

        compiler.push_scope(Some(symbol_table));
        compiler
    }

    fn get_scope(&self) -> &CompilerScope {
        &self.scopes[self.scope_index as usize]
    }

    fn get_instructions(&self) -> &Bytecode {
        &self.scopes[self.scope_index as usize].instructions
    }

    fn push_scope(&mut self, symbol_table: Option<Rc<RefCell<SymbolTable>>>) {
        self.scope_index += 1;
        self.scopes.push(
            CompilerScope{
                instructions: Vec::new(),
                last_instruction: CompiledInstruction {
                    opcode: Opcode::NotImplemented,
                    position: -1
                },
                previous_instruction: CompiledInstruction {
                    opcode: Opcode::NotImplemented,
                    position: -1
                }
            }
        );

        if let Some(mut symbol_table) = symbol_table {
            symbol_table.borrow_mut().parent = Some(self.symbol_table.clone());
        } else if self.symbol_table.borrow().kind == ScopeType::Native {
            let mut globals = SymbolTable::create_global_symbol_table(None);
            // TODO
            globals.parent = Some(self.symbol_table.clone());
            self.symbol_table = Rc::new(RefCell::new(globals));
        } else {
            let parent = Some(self.symbol_table.clone());
            let symbol_table = SymbolTable::new(ScopeType::Local, parent);
            self.symbol_table = Rc::new(RefCell::new(symbol_table));
        }
    }
}