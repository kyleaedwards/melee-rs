use std::{collections::HashMap, rc::Rc, cell::RefCell};
use crate::object::Object;

/// Label defining level of variable scope.
///
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ScopeType {
    Native,
    Global,
    Local,
    Free,
    Slf,
}

/// Symbol representing a scoped variable.
///
#[derive(Debug)]
pub struct SymbolIdentifier {
    pub label: String,
    pub index: i32,
    pub depth: i32,
    pub kind: ScopeType
}

/// Symbol table for tracking named variables through the
/// compilation process.
///
#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Rc<SymbolIdentifier>>,
    pub free_symbols: Vec<Rc<SymbolIdentifier>>,
    pub symbol_count: i32,
    pub depth: i32,
    pub kind: ScopeType,
    pub parent: Option<Rc<RefCell<SymbolTable>>>
}

static mut iota: i32 = 0;

/// Symbol table for tracking named variables through the
/// compilation process.
///
impl SymbolTable {
    pub fn new(kind: ScopeType, parent: Option<Rc<RefCell<SymbolTable>>>) -> SymbolTable {
        SymbolTable{
            symbols: HashMap::new(),
            free_symbols: Vec::new(),
            symbol_count: 0,
            depth: if parent.is_none() {
                -1
            } else {
                parent.as_ref().unwrap().borrow().depth + 1
            },
            kind,
            parent
        }
    }

    /// Adds a new iota symbol to the table and returns its unique index.
    ///
    pub fn add_iota(&mut self) -> i32 {
        let label = unsafe {
            iota += 1;
            format!("$iota__{}", iota)
        };
        self.add(label)
    }

    /// Adds a new symbol to the table and returns its unique index.
    ///
    pub fn add(&mut self, label: String) -> i32 {
        let label = label.clone();
        let index = self.symbol_count;
        let symbol = SymbolIdentifier{
            label: label.clone(),
            depth: self.depth,
            kind: self.kind,
            index
        };
        self.symbol_count += 1;
        self.symbols.insert(label, Rc::new(symbol));
        index
    }

    /// Save the name of the current closure as a symbol in scope.
    ///
    pub fn set_self(&mut self, label: String) -> Rc<SymbolIdentifier> {
        let symbol = SymbolIdentifier{
            label: label.clone(),
            index: 0,
            depth: -1,
            kind: ScopeType::Slf
        };
        let symbol = Rc::new(symbol);
        self.symbols.insert(label.clone(), symbol.clone());
        symbol
    }

    /// Free a variable for use in an inner function or closure.
    ///
    pub fn free(&mut self, symbol: Rc<SymbolIdentifier>) -> Rc<SymbolIdentifier> {
        self.free_symbols.push(symbol.clone());
        let free_symbol = SymbolIdentifier{
            label: symbol.label.clone(),
            index: self.free_symbols.len() as i32 - 1,
            depth: symbol.depth,
            kind: ScopeType::Free
        };
        let free_symbol = Rc::new(free_symbol);
        self.symbols.insert(symbol.label.clone(), free_symbol.clone());
        free_symbol
    }

    /// Look up a symbol in the table. If not found, it recurses
    /// up its parent scope.
    ///
    pub fn get(&mut self, label: &str) -> Option<Rc<SymbolIdentifier>> {
        if self.symbols.contains_key(label) && self.parent.is_some() {
            let variable = self.parent.as_ref()?.borrow_mut().get(label);
            if let Some(var) = variable {
                if var.kind == ScopeType::Global || var.kind == ScopeType::Native {
                    return Some(var);
                }
                return Some(self.free(var));
            }
        }
        let symbol = self.symbols.get(label)?;
        Some(symbol.clone())
    }

    /// Look up a symbol in the table and return its unique index.
    ///
    pub fn get_index(&mut self, label: &str) -> Option<i32> {
        Some(self.get(label)?.index)
    }

    /// Create a default global symbol table with native values
    /// already populated.
    ///
    pub fn create_global_symbol_table(builtins: Option<HashMap<String, Object>>) -> SymbolTable {
        let globals = SymbolTable::new(ScopeType::Global, None);
        // Add builtins
        globals
    }
}
