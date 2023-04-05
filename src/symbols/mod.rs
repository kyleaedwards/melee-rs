use std::collections::HashMap;
use crate::object::BaseObject;

/// Label defining level of variable scope.
///
#[derive(PartialEq, Eq)]
pub enum ScopeType {
    Native,
    Global,
    Local,
    Free,
    Slf,
}

/// Symbol representing a scoped variable.
///
pub struct SymbolIdentifier {
    label: String,
    index: i32,
    depth: i32,
    kind: ScopeType
}

/// Symbol table for tracking named variables through the
/// compilation process.
///
pub struct SymbolTable {
    symbols: HashMap<String, SymbolIdentifier>,
    pub free_symbols: Vec<SymbolIdentifier>,
    pub symbol_count: i32,
    pub depth: i32,
    pub kind: ScopeType,
    pub parent: Option<Box<SymbolTable>>
}

static mut iota: i32 = 0;

/// Symbol table for tracking named variables through the
/// compilation process.
///
impl SymbolTable {
    pub fn new(kind: ScopeType, parent: Option<SymbolTable>) -> SymbolTable {
        SymbolTable{
            symbols: HashMap::new(),
            free_symbols: Vec::new(),
            symbol_count: 0,
            depth: if parent.is_none() {
                -1
            } else {
                parent.as_ref().unwrap().depth + 1
            },
            kind,
            parent: if let Some(parent) = parent {
                Some(Box::new(parent))
            } else {
                None
            }
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
        let symbol = SymbolIdentifier{
            label: label.clone(),
            index: self.symbol_count,
            depth: self.depth,
            kind: self.kind
        };
        self.symbol_count += 1;
        self.symbols.insert(label, symbol);
        symbol.index
    }

    /// Save the name of the current closure as a symbol in scope.
    ///
    pub fn set_self(&mut self, label: String) -> SymbolIdentifier {
        let symbol = SymbolIdentifier{
            label: label.clone(),
            index: 0,
            depth: -1,
            kind: ScopeType::Slf
        };
        self.symbols.insert(label.clone(), symbol);
        symbol
    }

    /// Free a variable for use in an inner function or closure.
    ///
    pub fn free(&mut self, symbol: SymbolIdentifier) -> SymbolIdentifier {
        self.free_symbols.push(symbol);
        let free_symbol = SymbolIdentifier{
            label: symbol.label.clone(),
            index: self.free_symbols.len() as i32 - 1,
            depth: symbol.depth,
            kind: ScopeType::Free
        };
        self.symbols.insert(symbol.label.clone(), free_symbol);
        free_symbol
    }

    /// Look up a symbol in the table. If not found, it recurses
    /// up its parent scope.
    ///
    pub fn get(&mut self, label: String) -> Option<&SymbolIdentifier> {
        if self.symbols.contains_key(&label) && self.parent.is_some() {
            let mut variable = self.parent?.get(label);
            if variable.is_none() || variable?.kind == ScopeType::Global || variable?.kind == ScopeType::Native {
                return variable;
            }
            let symbol = variable.take().unwrap();
            return Some(&self.free(*symbol));
        }
        return self.symbols.get(&label);
    }

    /// Look up a symbol in the table and return its unique index.
    ///
    pub fn get_index(&mut self, label: String) -> Option<i32> {
        Some(self.get(label)?.index)
    }

    /// Create a default global symbol table with native values
    /// already populated.
    ///
    pub fn create_global_symbol_table(builtins: HashMap<String, BaseObject>) -> SymbolTable {
        let globals = SymbolTable::new(ScopeType::Global, None);
        // Add builtins
        globals
    }
}
