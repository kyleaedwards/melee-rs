use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{Program, StatementBlock, Statement, Expression};
use crate::bytecode::{Bytecode, Opcode, create_instruction, get_opcode_by_u8};
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

    /// Remove the topmost scope object and return its instructions.
    ///
    fn pop_scope(&mut self) -> Option<RefCell<Bytecode>> {
        if self.scope_index < 1 || self.symbol_table.borrow().parent.is_none() {
            return None;
        }

        let parent = self.symbol_table.borrow_mut().parent.take()?;
        self.symbol_table = parent;
        self.scope_index -= 1;
        
        let scope = self.scopes.pop()?;
        Some(scope.instructions)
    }

    /// Keeps track of a program constant and return a reference.
    ///
    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        return self.constants.len() - 1;
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
                    self.compile_expression(val)?;
                } else {
                    self.emit(Opcode::Null, 0, 0);
                }
                self.emit(Opcode::Return, 0, 0);
            }
            Statement::Yield { value, .. } => {
                if let Some(val) = value {
                    self.compile_expression(val)?;
                } else {
                    self.emit(Opcode::Null, 0, 0);
                }
                self.emit(Opcode::Yield, 0, 0);
            }
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                self.emit(Opcode::Pop, 0, 0);
            }
            Statement::Declare { name, value, .. } => {
                let symbol_table = self.symbol_table.clone();
                let mut symbol_table = symbol_table.borrow_mut();
                let index = symbol_table.add(name.value);
                self.compile_expression(value)?;
                let set_opcode = if symbol_table.kind == ScopeType::Global {
                    Opcode::SetGlobal
                } else {
                    Opcode::Set
                };
                self.emit(set_opcode, index, 0);
            }
            Statement::For { identifier, collection, block, .. } => {
                let symbol_table = self.symbol_table.clone();
                let mut symbol_table = symbol_table.borrow_mut();
                let position = symbol_table.add(identifier.value);
                let set_opcode = if symbol_table.kind == ScopeType::Global {
                    Opcode::SetGlobal
                } else {
                    Opcode::Set
                };
                let get_opcode = if symbol_table.kind == ScopeType::Global {
                    Opcode::GetGlobal
                } else {
                    Opcode::Get
                };
                let counter = symbol_table.add_iota();
                let collection_index = symbol_table.add_iota();

                let incr = self.add_constant(Object::Int(1)) as i32;
                let base = self.add_constant(Object::Int(0)) as i32;

                self.emit(Opcode::Const, base, 0);
                self.emit(set_opcode, counter, 0);

                // Save collection
                self.compile_expression(collection)?;
                self.emit(set_opcode, collection_index, 0);

                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                };

                self.loop_starts.push(instructions);
                self.breaks.push(Vec::new());

                // Check if iterator has gone past the end of the array
                self.emit(get_opcode, collection_index, 0);
                self.emit(Opcode::Len, 0, 0);
                self.emit(get_opcode, counter, 0);
                self.emit(Opcode::GreaterThan, 0, 0);

                let jump_out = self.emit(Opcode::JumpIfNot, 0xffff, 0) as usize;

                // Set the current array item in the local variable
                self.emit(get_opcode, collection_index, 0);
                self.emit(get_opcode, counter, 0);
                self.emit(Opcode::Index, 0, 0);
                self.emit(set_opcode, position, 0);

                // Increment the iterator
                self.emit(get_opcode, counter, 0);
                self.emit(Opcode::Const, incr, 0);
                self.emit(Opcode::Add, 0, 0);
                self.emit(set_opcode, counter, 0);

                // Compile code block and loop
                self.compile_statement_block(block)?;
                self.emit(Opcode::Jump, self.loop_starts[self.loop_starts.len() - 1] as i32, 0);

                // Replace jump-out condition with position after loop
                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                } as i32;
                self.replace_instruction(jump_out, instructions, 0);

                // Replace any break statements with position after loop
                let breaks = self.breaks.pop();
                if let Some(breaks) = breaks {
                    for break_pos in breaks {
                        self.replace_instruction(break_pos, instructions, 0);
                    }
                }

                self.loop_starts.pop();
            }
            Statement::While { condition, block, .. } => {
                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                };

                self.loop_starts.push(instructions);
                self.breaks.push(Vec::new());
      
                self.compile_expression(condition)?;
                let jump_out = self.emit(Opcode::JumpIfNot, 0xffff, 0) as usize;

                self.compile_statement_block(block)?;
                self.emit(Opcode::Jump, self.loop_starts[self.loop_starts.len() - 1] as i32, 0);
                
                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                } as i32;
                self.replace_instruction(jump_out, instructions, 0);

                if let Some(breaks) = self.breaks.pop() {
                    for break_pos in breaks {
                        self.replace_instruction(break_pos, instructions, 0);
                    }
                }

                self.loop_starts.pop();
            }
            Statement::Conditional { condition, consequence, alternative, .. } => {
                self.compile_expression(condition)?;

                // Jump to else clause (or outside of conditional statement if else doesn't exist).
                let jump_to_else = self.emit(Opcode::JumpIfNot, 0xffff, 0) as usize;
                self.compile_statement_block(consequence)?;
      
                let jump_out = self.emit(Opcode::Jump, 0xffff, 0) as usize;

                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                } as i32;
                self.replace_instruction(jump_to_else, instructions, 0);

                if let Some(alt) = alternative {
                    self.compile_statement_block(alt)?;
                }

                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                } as i32;
                self.replace_instruction(jump_out, instructions, 0);
            }
        };
        Ok(())
    }

    pub fn compile_expression(&mut self, expr: Box<Expression>) -> Result<(), CompilationError> {
        Err(CompilationError(String::from("Not implemented")))
    }

    /// Replaces an instruction in the program's bytecode.
    ///
    fn replace_instruction(&mut self, position: usize, op1: i32, op2: i32) {
        let scope = &self.scopes[self.scope_index as usize];
        let mut instructions = scope.instructions.borrow_mut();

        let opcode = get_opcode_by_u8(instructions[position]);
        let instruction = create_instruction(opcode, op1, op2);

        for i in 0..instruction.len() {
            instructions[position + i] = instruction[i];
        }
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
