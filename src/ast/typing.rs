use std::collections::HashMap;

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
