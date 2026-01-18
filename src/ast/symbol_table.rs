use std::collections::HashMap;
use std::fmt;

use crate::ast::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BuiltinDataType {
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
    Void,
}

impl BuiltinDataType {
    pub fn from_token(token: &Token) -> Option<Self> {
        Some(match &token.kind {
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
            TokenKind::Void => Self::Void,
            _ => return None,
        })
    }

    fn sizeof(&self) -> Option<u32> {
        Some(match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::F32 => 4,
            Self::F64 => 8,
            Self::Bool => 1,
            Self::Char => 1,
            Self::Str | Self::Void => return None,
        })
    }
    fn get_bigger_type(type1: &Self, type2: &Self) -> Self {
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

impl fmt::Display for BuiltinDataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructDataMember {
    pub name: String, // TODO(letohg): [2026-01-18] create type id system
    pub data_type: DataType,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DataType {
    Builtin(BuiltinDataType),
    Struct(String, Vec<StructDataMember>),
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl DataType {
    pub fn from_token(token: &Token) -> Self {
        match BuiltinDataType::from_token(token) {
            None => match token.kind {
                TokenKind::Identifier => Self::Struct(token.span.literal.clone(), vec![]),
                _ => todo!(),
            },
            Some(t) => Self::Builtin(t),
        }
    }

    fn sizeof(&self) -> Option<u32> {
        match self {
            Self::Builtin(t) => t.sizeof(),
            Self::Struct(_, _) => None,
        }
    }
    fn get_bigger_type(type1: &Self, type2: &Self) -> DataType {
        if type1.sizeof() > type2.sizeof() {
            return type1.clone();
        }
        return type2.clone();
    }

    fn is_builtin(&self) -> bool {
        match self {
            Self::Builtin(_) => true,
            _ => false,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        match self {
            Self::Builtin(t) => t.is_signed_integer(),
            _ => false,
        }
    }
    pub fn is_unsigned_integer(&self) -> bool {
        match self {
            Self::Builtin(t) => t.is_unsigned_integer(),
            _ => false,
        }
    }
    pub fn is_integer(&self) -> bool {
        self.is_signed_integer() || self.is_unsigned_integer()
    }

    pub fn is_floating(&self) -> bool {
        match self {
            Self::Builtin(t) => t.is_floating(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Builtin(t) => t.is_bool(),
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Builtin(t) => t.to_string(),
            Self::Struct(name, _members) => name.to_string(),
        }
    }

    pub fn get_common_type(type1: &Self, type2: &Self) -> Option<Self> {
        match (type1, type2) {
            (Self::Builtin(t1), Self::Builtin(t2)) => {
                let builtin_type = BuiltinDataType::get_common_type(t1, t2)?;
                Some(Self::Builtin(builtin_type))
            }
            _ => None,
        }
    }

    pub fn is_convertable_to(&self, wanted_type: Self) -> bool {
        match (self, wanted_type) {
            (Self::Builtin(t1), Self::Builtin(t2)) => t1.is_convertable_to(t2),
            _ => false,
        }
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

    pub fn get_type_definition_of(&self, name: &str) -> Option<&Symbol> {
        let var: &Symbol = self.lookup(name)?;
        match var {
            Symbol::Variable(st) | Symbol::Constant(st) => match &st.data_type {
                DataType::Struct(n, m) => {
                    // println!("Variable {} of type {}", _expr.identifier.name(), n);
                    return self.lookup(n);
                }
                _ => return None,
            },
            _ => None,
        }
    }
}
