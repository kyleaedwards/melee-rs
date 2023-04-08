use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{Program, StatementBlock, Statement, Expression, Prefix, Infix, CompoundAssign, Identifier};
use crate::bytecode::{Bytecode, Opcode, create_instruction, get_opcode_by_u8};
use crate::object::{Object, Callable};
use crate::symbols::{SymbolTable, ScopeType};
use crate::token::Token;

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
pub struct CompilationError(String, Token);

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CompilationError(msg, token) = self;
        write!(f, "Compilation error: {} at column {}, line {}", msg, token.column, token.line)
    }
}

pub struct Compiler {
    scopes: Vec<CompilerScope>,
    scope_index: i32, // TODO: Make these Option<usize> instead of negatives
    symbol_table: Rc<RefCell<SymbolTable>>,
    constants: Vec<Object>,
    loop_starts: Vec<usize>,
    breaks: Vec<Vec<usize>>
}

impl Compiler {
    pub fn new(constants: Vec<Object>, symbol_table: Option<Rc<RefCell<SymbolTable>>>) -> Compiler {
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

        if let Some(symbol_table) = symbol_table {
            compiler.push_scope(Some(symbol_table));
        } else {
            let parent = Some(compiler.symbol_table.clone());
            let global_symbols = SymbolTable::new(ScopeType::Global, parent);
            compiler.push_scope(Some(Rc::new(RefCell::new(global_symbols))));
        }
        compiler.scope_index = 0;
        compiler
    }
    
    pub fn get_instructions(&mut self) -> Bytecode {
        let scope = &self.scopes[self.scope_index as usize];
        scope.instructions.borrow().clone()
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

        if let Some(symbol_table) = symbol_table {
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
            if let Err(e) = self.compile_statement(&stmt) {
                return Err(e);
            }
        }
        Ok(())
    }

    pub fn compile_statement_block(&mut self, block: StatementBlock) -> Result<(), CompilationError> {
        for stmt in block {
            if let Err(e) = self.compile_statement(&stmt) {
                return Err(e);
            }
        }
        Ok(())
    }

    pub fn compile_ref_statement_block(&mut self, block: &StatementBlock) -> Result<(), CompilationError> {
        for stmt in block {
            if let Err(e) = self.compile_statement(stmt) {
                return Err(e);
            }
        }
        Ok(())
    }

    pub fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CompilationError> {
        match stmt {
            Statement::Break { .. } => {
                let pos = self.emit(Opcode::Jump, 0xffff, 0).clamp(0, i32::MAX) as usize;
                let last_index = self.breaks.len() - 1;
                self.breaks[last_index].push(pos);
            }
            Statement::Continue { token } => {
                if self.loop_starts.len() == 0 {
                    return Err(CompilationError(String::from("Cannot use continue outside of a loop"), token.clone()));
                }
                self.emit(
                    Opcode::Jump,
                    self.loop_starts[self.loop_starts.len() - 1].clamp(0, i32::MAX as usize) as i32,
                    0
                );
            }
            Statement::Return { value, .. } => {
                if let Some(val) = value {
                    self.compile_expression(&val)?;
                } else {
                    self.emit(Opcode::Null, 0, 0);
                }
                self.emit(Opcode::Return, 0, 0);
            }
            Statement::Yield { value, .. } => {
                if let Some(val) = value {
                    self.compile_expression(&val)?;
                } else {
                    self.emit(Opcode::Null, 0, 0);
                }
                self.emit(Opcode::Yield, 0, 0);
            }
            Statement::Expression(expr) => {
                self.compile_expression(&expr)?;
                self.emit(Opcode::Pop, 0, 0);
            }
            Statement::Declare { name, value, .. } => {
                let symbol_table = self.symbol_table.clone();
                let mut symbol_table = symbol_table.borrow_mut();
                let index = symbol_table.add(name.value.clone());
                self.compile_expression(&value)?;
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
                let position = symbol_table.add(identifier.value.clone());
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
                self.compile_expression(&collection)?;
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
                self.compile_ref_statement_block(block)?;
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
      
                self.compile_expression(&condition)?;
                let jump_out = self.emit(Opcode::JumpIfNot, 0xffff, 0) as usize;

                self.compile_ref_statement_block(block)?;
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
                self.compile_expression(&condition)?;

                // Jump to else clause (or outside of conditional statement if else doesn't exist).
                let jump_to_else = self.emit(Opcode::JumpIfNot, 0xffff, 0) as usize;
                self.compile_ref_statement_block(consequence)?;
      
                let jump_out = self.emit(Opcode::Jump, 0xffff, 0) as usize;

                let instructions = {
                    let scope = &self.scopes[self.scope_index as usize];
                    scope.instructions.borrow().len()
                } as i32;
                self.replace_instruction(jump_to_else, instructions, 0);

                if let Some(alt) = alternative {
                    self.compile_ref_statement_block(alt)?;
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

    pub fn compile_identifier(&mut self, ident: &Identifier) -> Result<(), CompilationError> {
        let symbol_table = self.symbol_table.clone();
        let symbol = symbol_table.borrow_mut().get(&ident.value);

        let (symbol_kind, symbol_index) = if let Some(symbol) = symbol {
            let symbol_ref = symbol.as_ref();
            (symbol_ref.kind, symbol_ref.index)
        } else {
            return Err(CompilationError(
                format!("Attempting to use undefined variable {}", &ident.value),
                ident.token.clone()
            ))
        };

        let opcode = match symbol_kind {
            ScopeType::Free => Opcode::GetClosure,
            ScopeType::Native => Opcode::GetNative,
            ScopeType::Global => Opcode::GetGlobal,
            ScopeType::Slf => Opcode::SelfClosure,
            _ => Opcode::Get
        };
        self.emit(opcode, symbol_index, 0);
        Ok(())
    }

    pub fn compile_callable(&mut self, repr: String, generator: bool, token: &Token, parameters: &Vec<Identifier>, body: &Vec<Statement>) -> Result<(), CompilationError> {
        self.push_scope(None);
        for param in parameters {
            self.symbol_table.borrow_mut().add(param.value.clone());
        }
        self.compile_ref_statement_block(body)?;

        let (free_symbols, num_symbols) = {
            let symbol_table = self.symbol_table.borrow_mut();
            let free_symbols = symbol_table.free_symbols.clone();
            let num_symbols = symbol_table.symbol_count;
            (free_symbols, num_symbols)
        };

        let scope = &self.scopes[self.scope_index as usize];

        // Implicit null return if not explicit
        if scope.last_instruction.opcode != Opcode::Return {
            self.emit(Opcode::Null, 0, 0);
            self.emit(Opcode::Return, 0, 0);
        }

        let instructions = if let Some(instructions) = self.pop_scope() {
            instructions
        } else {
            return Err(CompilationError(
                String::from("Error compiling function"),
                token.clone()
            ));
        };

        let symbol_length = free_symbols.len();
        for symbol in free_symbols {
            let symbol = symbol.clone();
            let opcode = match symbol.kind {
                ScopeType::Free => Opcode::GetClosure,
                ScopeType::Native => Opcode::GetNative,
                ScopeType::Global => Opcode::GetGlobal,
                ScopeType::Slf => Opcode::SelfClosure,
                _ => Opcode::Get
            };
            self.emit(opcode, symbol.index, 0);
        }

        let fn_or_gen = if generator {
            Callable::Gen {
                instructions: instructions.borrow_mut().to_vec(),
                repr,
                num_locals: num_symbols as usize,
                num_params: parameters.len()
            }
        } else {
            Callable::Fn {
                instructions: instructions.borrow_mut().to_vec(),
                repr,
                num_locals: num_symbols as usize,
                num_params: parameters.len()
            }
        };
        let callable = Object::Callable(fn_or_gen);

        let idx = self.add_constant(callable) as i32;
        self.emit(Opcode::Closure, idx, symbol_length as i32);

        Ok(())
    }

    pub fn compile_expression(&mut self, expr: &Box<Expression>) -> Result<(), CompilationError> {
        match expr.as_ref() {
            Expression::Boolean { value, .. } => {
                self.emit(if *value { Opcode::True } else { Opcode::False }, 0, 0);
            }
            Expression::Integer { value, .. } => {
                let constant_index = self.add_constant(Object::Int(*value)) as i32;
                self.emit(Opcode::Const, constant_index, 0);
            }
            Expression::Identifier(ident) => {
                self.compile_identifier(ident);
            }
            Expression::Prefix { operator, right, .. } => {
                self.compile_expression(right)?;
                match *operator {
                    Prefix::Minus => {
                        self.emit(Opcode::Minus, 0, 0);
                    }
                    Prefix::Plus => {
                        // Little hacky but this operator is meaningless anyways
                        self.emit(Opcode::Minus, 0, 0);
                        self.emit(Opcode::Minus, 0, 0);
                    }
                    Prefix::Not => {
                        self.emit(Opcode::Bang, 0, 0);
                    }
                }
            }
            Expression::Infix { left, operator, right, .. } => {
                if *operator == Infix::LessThan || *operator == Infix::LessThanEquals {
                    self.compile_expression(right)?;
                    self.compile_expression(left)?;
                } else {
                    self.compile_expression(left)?;
                    self.compile_expression(right)?;
                }
                let opcode = match *operator {
                    Infix::Add => Opcode::Add,
                    Infix::Subtract => Opcode::Subtract,
                    Infix::Multiply => Opcode::Multiply,
                    Infix::Divide => Opcode::Divide,
                    Infix::Modulus => Opcode::Modulus,
                    Infix::Equals => Opcode::Equals,
                    Infix::NotEquals => Opcode::NotEquals,
                    Infix::GreaterThan => Opcode::GreaterThan,
                    Infix::GreaterThanEquals => Opcode::GreaterThanEquals,
                    Infix::LessThan => Opcode::GreaterThan,
                    Infix::LessThanEquals => Opcode::GreaterThanEquals,
                    Infix::And => Opcode::And,
                    Infix::Or => Opcode::Or
                };
                self.emit(opcode, 0, 0);
            }
            Expression::CompoundAssign { token, operator, left, value } => {
                let opcode = match *operator {
                    CompoundAssign::Add => Opcode::Add,
                    CompoundAssign::Subtract => Opcode::Subtract,
                    CompoundAssign::Multiply => Opcode::Multiply,
                    CompoundAssign::Divide => Opcode::Divide,
                    CompoundAssign::Modulus => Opcode::Modulus
                };
                match left.as_ref() {
                    Expression::Index { collection, index, .. } => {
                        // Left identifier
                        self.compile_expression(collection)?;
                        self.compile_expression(index)?;
                        // Left value
                        self.compile_expression(collection)?;
                        self.compile_expression(index)?;
                        self.emit(Opcode::Index, 0, 0);
                        // Right value
                        self.compile_expression(value)?;
                        // Operation
                        self.emit(opcode, 0, 0);
                        // Assignment
                        self.emit(Opcode::SetIndex, 0, 0);
                        // Return value
                        self.compile_expression(collection)?;
                        self.compile_expression(index)?;
                        self.emit(Opcode::Index, 0, 0);

                    },
                    Expression::Identifier(ident) => {
                        let symbol_table = self.symbol_table.clone();
                        let symbol = symbol_table.borrow_mut().get(&ident.value);
                        let (symbol_kind, symbol_index) = if let Some(symbol) = symbol {
                            let symbol_ref = symbol.as_ref();
                            (symbol_ref.kind, symbol_ref.index)
                        } else {
                            return Err(CompilationError(
                                format!("Cannot assign undefined variable {}", ident.value),
                                ident.token.clone()
                            ))
                        };
                        let (setter, getter) = match symbol_kind {
                            ScopeType::Free => (Opcode::SetClosure, Opcode::GetClosure),
                            ScopeType::Local => (Opcode::Set, Opcode::Get),
                            ScopeType::Global => (Opcode::SetGlobal, Opcode::GetGlobal),
                            _ => {
                                return Err(CompilationError(
                                    String::from("Unsupported assignment operation"),
                                    ident.token.clone()
                                ));
                            }
                        };
                        // Left value
                        self.emit(getter, symbol_index, 0);
                        // Right value
                        self.compile_expression(value)?;
                        // Operation
                        self.emit(opcode, 0, 0);
                        self.emit(setter, symbol_index, 0);
                        // Return value
                        self.emit(getter, symbol_index, 0);
                    },
                    _ => {
                        return Err(CompilationError(
                            String::from("Left hand of assignment must be a variable or array index expression"),
                            token.clone()
                        ));
                    }
                }
            }
            Expression::Assign { token, left, value } => {
                match left.as_ref() {
                    Expression::Index { collection, index, .. } => {
                        // Left identifier
                        self.compile_expression(collection)?;
                        self.compile_expression(index)?;
                        // Right value
                        self.compile_expression(value)?;
                        // Assignment
                        self.emit(Opcode::SetIndex, 0, 0);
                        // Return value
                        self.compile_expression(collection)?;
                        self.compile_expression(index)?;
                        self.emit(Opcode::Index, 0, 0);
                    },
                    Expression::Identifier(ident) => {
                        let symbol_table = self.symbol_table.clone();
                        let symbol = symbol_table.borrow_mut().get(&ident.value);
                        let (symbol_kind, symbol_index) = if let Some(symbol) = symbol {
                            let symbol_ref = symbol.as_ref();
                            (symbol_ref.kind, symbol_ref.index)
                        } else {
                            return Err(CompilationError(
                                format!("Cannot assign undefined variable {}", ident.value),
                                ident.token.clone()
                            ))
                        };
                        let (setter, getter) = match symbol_kind {
                            ScopeType::Free => (Opcode::SetClosure, Opcode::GetClosure),
                            ScopeType::Local => (Opcode::Set, Opcode::Get),
                            ScopeType::Global => (Opcode::SetGlobal, Opcode::GetGlobal),
                            _ => {
                                return Err(CompilationError(
                                    String::from("Unsupported assignment operation"),
                                    ident.token.clone()
                                ));
                            }
                        };
                        self.compile_expression(value)?;
                        self.emit(setter, symbol_index, 0);
                        // Return value
                        self.emit(getter, symbol_index, 0);
                    },
                    _ => {
                        return Err(CompilationError(
                            String::from("Left hand of assignment must be a variable or array index expression"),
                            token.clone()
                        ));
                    }
                }
            }
            Expression::Array { values, .. } => {
                for value in values {
                    self.compile_expression(value)?;
                }
                self.emit(Opcode::Array, values.len() as i32, 0);
            }
            Expression::Index { collection, index, .. } => {
                self.compile_expression(collection)?;
                self.compile_expression(index)?;
                self.emit(Opcode::Index, 0, 0);
            }
            Expression::Next { token, value } => {
                if let Err(e) = self.compile_expression(value) {
                    return Err(CompilationError(
                        String::from("Cannot use the `next` keyword without an operand"),
                        token.clone()
                    ))
                }
                self.emit(Opcode::Next, 0, 0);
            }
            Expression::Function { token, parameters, body } => {
                self.compile_callable(
                    format!("{}", expr),
                    false,
                    token,
                    parameters,
                    body
                )?;
            }
            Expression::Generator { token, parameters, body } => {
                self.compile_callable(
                    format!("{}", expr),
                    true,
                    token,
                    parameters,
                    body
                )?;
            }
            Expression::Call { callee, arguments, .. } => {
                self.compile_expression(callee)?;
                for arg in arguments {
                    self.compile_expression(arg)?;
                }
                self.emit(Opcode::Call, arguments.len() as i32, 0);
            }
            Expression::Note { token, arguments } => {
                if arguments.len() < 1 {
                    return Err(CompilationError(
                      String::from("Cannot use the `note` keyword without arguments"),
                      token.clone(),
                    ));
                }
                for arg in arguments {
                    self.compile_expression(arg)?;
                }
                self.emit(Opcode::Note, arguments.len() as i32, 0);
            }
            Expression::ControlChange { token, arguments } => {
                if arguments.len() < 1 {
                    return Err(CompilationError(
                      String::from("Cannot use the `cc` keyword without arguments"),
                      token.clone(),
                    ));
                }
                for arg in arguments {
                    self.compile_expression(arg)?;
                }
                self.emit(Opcode::Control, arguments.len() as i32, 0);
            }
            Expression::Rest { token, arguments } => {
                if arguments.len() < 1 {
                    return Err(CompilationError(
                      String::from("Cannot use the `rest` keyword without arguments"),
                      token.clone(),
                    ));
                }
                for arg in arguments {
                    self.compile_expression(arg)?;
                }
                self.emit(Opcode::Rest, arguments.len() as i32, 0);
            }
        }
        Ok(())
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
