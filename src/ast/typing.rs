use std::collections::HashMap;

use crate::ast::lexer::{Token, TokenKind};

pub type TypeId = usize;

pub enum TypeKind {
    Builtin(BuiltinType),
    Struct,
}

pub struct TypeInfo {
    kind: TypeKind,
}

pub struct TypeTable {
    types: Vec<TypeInfo>,
    builtins: HashMap<BuiltinType, TypeId>,
}

impl TypeTable {
    pub fn new() -> Self {
        let mut types = Vec::new();
        let mut builtins = HashMap::new();

        for builtin in [
            BuiltinType::I8,
            BuiltinType::I16,
            BuiltinType::I32,
            BuiltinType::I64,
            BuiltinType::U8,
            BuiltinType::U16,
            BuiltinType::U32,
            BuiltinType::U64,
            BuiltinType::F32,
            BuiltinType::F64,
            BuiltinType::Bool,
            BuiltinType::Char,
            BuiltinType::Str,
            BuiltinType::Void,
        ] {
            let id = types.len();
            types.push(TypeInfo {
                kind: TypeKind::Builtin(builtin),
            });
            builtins.insert(builtin, id);
        }

        Self { types, builtins }
    }

    pub fn get_builtin(&self, builtin: BuiltinType) -> Option<TypeId> {
        self.builtins.get(&builtin).cloned()
    }

    pub fn get_builtin_from_token(&self, token: &Token) -> Option<TypeId> {
        BuiltinType::from_token(token).and_then(|b| self.get_builtin(b))
    }

    pub fn is_boolean(&self, type_id: TypeId) -> bool {
        matches!(
            self.types.get(type_id).map(|t| &t.kind),
            Some(TypeKind::Builtin(BuiltinType::Bool))
        )
    }

    pub fn is_integer(&self, type_id: TypeId) -> bool {
        matches!(
            self.types.get(type_id).map(|t| &t.kind),
            Some(TypeKind::Builtin(b)) if matches!(b, BuiltinType::I8 | BuiltinType::I16 | BuiltinType::I32 | BuiltinType::I64 | BuiltinType::U8 | BuiltinType::U16 | BuiltinType::U32 | BuiltinType::U64)
        )
    }

    pub fn is_float(&self, type_id: TypeId) -> bool {
        matches!(
            self.types.get(type_id).map(|t| &t.kind),
            Some(TypeKind::Builtin(BuiltinType::F32 | BuiltinType::F64))
        )
    }

    pub fn is_numeric(&self, type_id: TypeId) -> bool {
        self.is_integer(type_id) || self.is_float(type_id)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub enum BuiltinType {
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

impl BuiltinType {
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
}
