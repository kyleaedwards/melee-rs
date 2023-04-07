use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{Program, StatementBlock, Statement};
use crate::bytecode::{Bytecode, Opcode, create_instruction, Operation};
use crate::object::Object;
use crate::symbols::{SymbolTable, ScopeType};

struct CompiledInstruction {
    pub opcode: Opcode,
    pub position: i32, // TODO: Make these Option<usize> instead of negatives
}

struct CompilerScope {
    /// Serial bytecode instructions representing a program or function body.
    ///
    pub instructions: RefCell<Bytecode>,

    /// Saved instruction to backtrack or remove previous items from the bytecode.
    /// This is primarily used to support implicit returns from block statements.
    ///
    pub last_instruction: CompiledInstruction,

    /// Saved instruction to backtrack or remove previous items from the bytecode.
    /// This is primarily used to support implicit returns from block statements.
    ///
    pub previous_instruction: CompiledInstruction
}

#[derive(Debug, Clone)]
struct CompilationError(String);

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CompilationError(msg) = self;
        write!(f, "Compilation error: {}", msg)
    }
}

struct Compiler {
    scopes: Vec<CompilerScope>,
    scope_index: i32, // TODO: Make these Option<usize> instead of negatives
    symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Object>,
    loop_starts: Vec<usize>,
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
            scope_index: 0,
            symbol_table: Rc::new(RefCell::new(base_symbol_table)),
            constants,
            loop_starts: Vec::new(),
            breaks: Vec::new()
        };

        compiler.push_scope(Some(symbol_table));
        compiler.scope_index = 0;
        compiler
    }

    fn push_scope(&mut self, symbol_table: Option<Rc<RefCell<SymbolTable>>>) {
        self.scope_index += 1;
        self.scopes.push(
            CompilerScope{
                instructions: RefCell::new(Vec::new()),
                last_instruction: CompiledInstruction {
                    opcode: Opcode::NotImplemented,
                    position: -1 // TODO: Make these Option<usize> instead of negatives
                },
                previous_instruction: CompiledInstruction {
                    opcode: Opcode::NotImplemented,
                    position: -1 // TODO: Make these Option<usize> instead of negatives
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

    pub fn compile_program(&mut self, program: Program) -> Result<(), CompilationError> {
        for stmt in program.statements {
            if let Err(e) = self.compile_statement(stmt) {
                return Err(e);
            }
        }
        Ok(())
    }

    pub fn compile_statement_block(&mut self, block: StatementBlock) -> Result<(), CompilationError> {
        for stmt in block {
            if let Err(e) = self.compile_statement(stmt) {
                return Err(e);
            }
        }
        Ok(())
    }

    pub fn compile_statement(&mut self, stmt: Statement) -> Result<(), CompilationError> {
        match stmt {
            Statement::Break { .. } => {
                let pos = self.emit(Opcode::Jump, 0xffff, 0).clamp(0, i32::MAX) as usize;
                let last_index = self.breaks.len() - 1;
                self.breaks[last_index].push(pos);
            }
            Statement::Continue { .. } => {
                if self.loop_starts.len() == 0 {
                    return Err(CompilationError(String::from("Cannot use continue outside of a loop")));
                }
                self.emit(
                    Opcode::Jump,
                    self.loop_starts[self.loop_starts.len() - 1].clamp(0, i32::MAX as usize) as i32,
                    0
                );
            }
            Statement::Return { value, .. } => {
                if let Some(val) = value {
                    self.compile_expression(val);
                } else {
                    self.emit(Opcode::Null, 0, 0);
                }
                self.emit(Opcode::Return, 0, 0);
            }
            Statement::Yield { value, .. } => {
                if let Some(val) = value {
                    self.compile_expression(val);
                } else {
                    self.emit(Opcode::Null, 0, 0);
                }
                self.emit(Opcode::Yield, 0, 0);
            }
            _ => panic!("Not implemented")
        };
        Ok(())
    }

    /// Add an instruction to the program's bytecode.
    ///
    fn emit(&mut self, opcode: Opcode, op1: i32, op2: i32) -> i32 {
        let mut scope = &mut self.scopes[self.scope_index as usize];

        let mut instructions = scope.instructions.borrow_mut();
        let position = instructions.len() as i32;

        let mut instruction = create_instruction(opcode, op1, op2);
        instructions.append(&mut instruction);
        
        scope.previous_instruction.opcode = scope.last_instruction.opcode;
        scope.previous_instruction.position = scope.last_instruction.position;
        scope.last_instruction.opcode = opcode;
        scope.last_instruction.position = position;
        position
    }
}
