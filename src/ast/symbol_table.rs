use std::collections::HashMap;
use std::fmt;

use crate::ast::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    Str,
    Struct(String),
    Void,
}
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl DataType {
    pub fn from_token(token: &Token) -> Self {
        match &token.kind {
            TokenKind::I8 => Self::I8,
            TokenKind::I16 => Self::I16,
            TokenKind::I32 => Self::I32,
            TokenKind::I64 => Self::I64,
            TokenKind::U8 => Self::U8,
            TokenKind::U16 => Self::U16,
            TokenKind::U32 => Self::U32,
            TokenKind::U64 => Self::U64,
            TokenKind::F32 => Self::F32,
            TokenKind::F64 => Self::F64,
            TokenKind::Bool => Self::Bool,
            TokenKind::Char => Self::Char,
            TokenKind::Str => Self::Str,
            TokenKind::Identifier => Self::Struct(token.span.literal.clone()),
            TokenKind::Void => Self::Void,
            _ => todo!(),
        }
    }
    fn sizeof(&self) -> Option<u32> {
        Some(match self {
            DataType::I8 => 1,
            DataType::I16 => 2,
            DataType::I32 => 4,
            DataType::I64 => 8,
            DataType::U8 => 1,
            DataType::U16 => 2,
            DataType::U32 => 4,
            DataType::U64 => 8,
            DataType::F32 => 4,
            DataType::F64 => 8,
            DataType::Bool => 1,
            DataType::Char => 1,
            DataType::Str | DataType::Struct(_) | DataType::Void => return None,
        })
    }
    fn get_bigger_type(type1: &Self, type2: &Self) -> DataType {
        if type1.sizeof() > type2.sizeof() {
            return type1.clone();
        }
        return type2.clone();
    }
    pub fn is_signed_integer(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 => true,
            _ => false,
        }
    }
    pub fn is_unsigned_integer(&self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => true,
            _ => false,
        }
    }
    pub fn is_integer(&self) -> bool {
        self.is_signed_integer() || self.is_unsigned_integer()
    }

    pub fn is_floating(&self) -> bool {
        match self {
            Self::F32 | Self::F64 => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::I8
            | Self::I16
            | Self::I32
            | Self::I64
            | Self::U8
            | Self::U16
            | Self::U32
            | Self::U64
            | Self::F32
            | Self::F64
            | Self::Bool => true,
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Bool => "bool",
            Self::Char => "char",
            Self::Str => "str",
            Self::Struct(name) => name,
            Self::Void => "void",
        }
        .to_string()
    }

    pub fn get_common_type(type1: &Self, type2: &Self) -> Option<Self> {
        if type1.is_signed_integer() && type2.is_signed_integer()
            || type1.is_unsigned_integer() && type2.is_unsigned_integer()
            || type1.is_floating() && type2.is_floating()
        {
            return Some(Self::get_bigger_type(type1, type2));
        }
        None
    }

    pub fn is_convertable_to(&self, wanted_type: Self) -> bool {
        if *self == wanted_type {
            return true;
        }
        if self.is_signed_integer() && wanted_type.is_signed_integer()
            || self.is_unsigned_integer() && wanted_type.is_unsigned_integer()
            || self.is_floating() && wanted_type.is_floating()
        {
            return self.sizeof() <= wanted_type.sizeof();
        }
        false
    }
}

#[derive(Debug)]
pub struct VariableInfo {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub name: String,
    pub parameters: Vec<DataType>, // Simplified
    pub return_type: DataType,
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
    pub return_type: DataType,
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

    pub fn declare_global_identifier(&mut self, symbol: Symbol) {
        if self.global_scope.contains_key(&symbol.name()) {
            // TODO(letohg): [2025-07-18] output diagnostic message
            // return Err(format!("Redefinition of global symbol `{}`", symbol.name));
        }

        self.global_scope.insert(symbol.name(), symbol);
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
