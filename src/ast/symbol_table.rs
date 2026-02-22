use std::collections::HashMap;

use crate::ast::typing::TypeId;

#[derive(Debug)]
pub struct VariableInfo {
    pub name: String,
    pub data_type: TypeId,
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub name: String,
    pub parameters: Vec<TypeId>, // Simplified
    pub return_type: TypeId,
}

pub enum Symbol {
    Variable(VariableInfo),
    Constant(VariableInfo),
    Function(FunctionInfo),
}
impl Symbol {
    pub fn name(&self) -> String {
        match &self {
            Symbol::Variable(a) => a.name.clone(),
            Symbol::Constant(a) => a.name.clone(),
            Symbol::Function(a) => a.name.clone(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionContext {
    pub name: String,
    pub return_type: TypeId,
}

pub struct SymbolTable {
    global_scope: HashMap<String, Symbol>,
    scopes: Vec<HashMap<String, Symbol>>,
    pub function_stack: Vec<FunctionContext>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            global_scope: HashMap::new(),
            scopes: Vec::new(),
            function_stack: Vec::new(),
        }
    }
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_global_identifier(&mut self, symbol: Symbol) -> bool {
        if self.global_scope.contains_key(&symbol.name()) {
            // TODO(letohg): [2025-07-18] output diagnostic message
            // return Err(format!("Redefinition of global symbol `{}`", symbol.name()));
            return false;
        }

        self.global_scope.insert(symbol.name(), symbol);
        true
    }
    pub fn declare_local_identifier(&mut self, symbol: Symbol) {
        let scope = self.scopes.last_mut().expect("No scope available");
        if scope.contains_key(&symbol.name()) {
            // TODO(letohg): [2025-07-18] output diagnostic message
            // return Err(format!("Redefinition of symbol `{}`", symbol.name));
        }
        scope.insert(symbol.name(), symbol);
    }

    pub fn is_identifier_in_current_scope(&self, name: &str) -> bool {
        let scope = self.scopes.last().expect("No scope available");
        scope.contains_key(name)
    }

    pub fn is_global_scope(&self) -> bool {
        return self.scopes.len() == 0;
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        self.global_scope.get(name)
    }
}
