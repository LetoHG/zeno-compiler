use std::collections::HashMap;

use nerd_font_symbols::seti::SETI_COLDFUSION;

use crate::diagnostics::DiagnosticsColletionCell;

use super::{
    lexer::{Token, TokenKind},
    ASTVisitor,
};

#[derive(Clone, Eq, PartialEq)]
enum DataType {
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

impl DataType {
    fn from_token(token: &Token) -> DataType {
        match &token.kind {
            TokenKind::I8 => DataType::I8,
            TokenKind::I16 => DataType::I16,
            TokenKind::I32 => DataType::I32,
            TokenKind::I64 => DataType::I64,
            TokenKind::U8 => DataType::U8,
            TokenKind::U16 => DataType::U16,
            TokenKind::U32 => DataType::U32,
            TokenKind::U64 => DataType::U64,
            TokenKind::F32 => DataType::F32,
            TokenKind::F64 => DataType::F64,
            TokenKind::Bool => DataType::Bool,
            TokenKind::Char => DataType::Char,
            TokenKind::Str => DataType::Str,
            TokenKind::Struct => DataType::Struct(token.span.literal.clone()),
            TokenKind::Void => DataType::Void,
            _ => todo!(),
        }
    }

    fn is_integer(&self) -> bool {
        match self {
            DataType::I8
            | DataType::I16
            | DataType::I32
            | DataType::I64
            | DataType::U8
            | DataType::U16
            | DataType::U32
            | DataType::U64 => true,
            _ => false,
        }
    }

    fn is_floating(&self) -> bool {
        match self {
            DataType::F32 | DataType::F64 => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            DataType::I8
            | DataType::I16
            | DataType::I32
            | DataType::I64
            | DataType::U8
            | DataType::U16
            | DataType::U32
            | DataType::U64
            | DataType::F32
            | DataType::F64
            | DataType::Bool => true,
            _ => false,
        }
    }

    fn to_string(&self) -> String {
        match self {
            DataType::I8 => "i8",
            DataType::I16 => "i16",
            DataType::I32 => "i32",
            DataType::I64 => "i64",
            DataType::U8 => "u8",
            DataType::U16 => "u16",
            DataType::U32 => "u32",
            DataType::U64 => "u64",
            DataType::F32 => "f32",
            DataType::F64 => "f64",
            DataType::Bool => "bool",
            DataType::Char => "char",
            DataType::Str => "str",
            DataType::Struct(name) => name,
            DataType::Void => "void",
        }
        .to_string()
    }
}

#[derive(Clone)]
enum TypeInfo {
    Function(Vec<DataType>, DataType),
    Variable(DataType),
}

pub struct TypeChecker {
    scopes: Vec<HashMap<String, TypeInfo>>,
    diagnostics: DiagnosticsColletionCell,
    last_expr_data_type: DataType,
}

impl TypeChecker {
    pub fn new(diagnostics: DiagnosticsColletionCell) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            diagnostics,
            last_expr_data_type: DataType::Void,
        }
    }

    fn get_active_scope(&self) -> &HashMap<String, TypeInfo> {
        self.scopes.last().unwrap()
    }

    fn enter_scope(&mut self, scope_variables: HashMap<String, TypeInfo>) {
        self.scopes.push(scope_variables);
    }

    fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_identifier_to_scope(&mut self, identifier: &String, type_info: TypeInfo) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(identifier.clone(), type_info);
    }

    fn check_identifier_in_scope(&self, identifier: &String) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(identifier) {
                return true;
            }
        }
        return false;
    }

    fn get_identifier_in_scope(&self, identifier: &String) -> Option<&TypeInfo> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(identifier) {
                return scope.get(identifier);
            }
        }
        None
    }
}

impl ASTVisitor<()> for TypeChecker {
    fn visit_return_statement(&mut self, statement: &super::ASTReturnStatement) {
        todo!()
    }

    fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) -> () {
        for statement in statement.statements.iter() {
            self.visit_statement(statement);
        }
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) {
        self.add_identifier_to_scope(
            &statement.identifier.span.literal,
            TypeInfo::Variable(DataType::from_token(&statement.data_type)),
        );
    }

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) {
        self.add_identifier_to_scope(
            &statement.identifier.span.literal,
            TypeInfo::Variable(DataType::from_token(&statement.data_type)),
        );
    }

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) {
        todo!()
    }

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) {
        todo!()
    }

    fn visit_while_loop_statement(&mut self, statement: &super::ASTWhileStatement) {
        todo!()
    }

    fn visit_function_statement(&mut self, function: &super::ASTFunctionStatement) {}

    fn visit_assignment_expression(&mut self, expr: &super::ASTAssignmentExpression) {
        todo!()
    }

    fn visit_function_call_expression(&mut self, expr: &super::ASTFunctionCallExpression) {
        if !self.check_identifier_in_scope(&expr.identifier().to_string()) {
            self.diagnostics
                .borrow_mut()
                .report_undefined_identifier(expr.identifier.span.clone());
            return;
        }

        let function = self
            .get_identifier_in_scope(&expr.identifier().to_string())
            .unwrap()
            .clone();

        match function {
            TypeInfo::Function(argument_types, return_type) => {
                if argument_types.len() != expr.arguments.len() {
                    self.diagnostics
                        .borrow_mut()
                        .report_number_of_function_arguments_mismatch(
                            expr.identifier.span.clone(),
                            argument_types.len(),
                            expr.arguments.len(),
                        );
                    return;
                }

                for (arg, expected_data_type) in expr.arguments.iter().zip(argument_types) {
                    self.visit_expression(arg);
                    if self.last_expr_data_type != expected_data_type {
                        self.diagnostics.borrow_mut().report_type_mismatch(
                            expr.identifier.span.clone(), // todo better position
                            self.last_expr_data_type.to_string(),
                            expected_data_type.to_string(),
                        );
                    }
                }

                self.last_expr_data_type = return_type.clone();
            }
            TypeInfo::Variable(_) => {
                self.diagnostics
                    .borrow_mut()
                    .report_not_a_callable(expr.identifier.span.clone());
                return;
            }
        }
    }

    fn visit_variable_expression(&mut self, expr: &super::ASTVariableExpression) {
        if !self.check_identifier_in_scope(&expr.identifier().to_string()) {
            self.diagnostics
                .borrow_mut()
                .report_undefined_variable(expr.identifier.span.clone());
        }

        let variable = self
            .get_identifier_in_scope(&expr.identifier().to_string())
            .unwrap()
            .clone();

        match variable {
            TypeInfo::Function(vec, data_type) => {
                // self.diagnostics
                //     .borrow_mut()
                //     .report_foun_callable(expr.identifier.span.clone());
                return;
            }
            TypeInfo::Variable(data_type) => {
                self.last_expr_data_type = data_type;
                return;
            }
        }
    }

    fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) {
        self.visit_expression(&expr.expr);

        match expr.operator.kind {
            super::ASTUnaryOperatorKind::Minus => {
                if !(self.last_expr_data_type.is_integer()
                    || self.last_expr_data_type.is_floating())
                {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Unary Operator needs int or float"),
                        expr.operator.token.span.clone(),
                    );
                    self.last_expr_data_type = DataType::Void;
                }
            }
            super::ASTUnaryOperatorKind::BitwiseNOT => {
                if !self.last_expr_data_type.is_integer() {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Unary Operator needs int"),
                        expr.operator.token.span.clone(),
                    );
                    self.last_expr_data_type = DataType::Void;
                }
            }
            super::ASTUnaryOperatorKind::LogicNot => {
                if !self.last_expr_data_type.is_bool() {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Unary Operator needs bool expression"),
                        expr.operator.token.span.clone(),
                    );
                    self.last_expr_data_type = DataType::Void;
                }
            }
        };
    }

    fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) {
        todo!()
    }

    fn visit_parenthesised_expression(&mut self, expr: &super::ASTParenthesizedExpression) {
        todo!()
    }

    fn visit_binary_operator(&mut self, op: &super::ASTBinaryOperator) {
        todo!()
    }

    fn visit_error(&mut self, span: &super::lexer::TextSpan) -> () {
        todo!()
    }

    fn visit_integer(&mut self, integer: &i64) {
        todo!()
    }
    fn visit_boolean(&mut self, boolean: bool) {
        todo!()
    }
    fn visit_string_literal(&mut self, string: &String) {
        todo!()
    }

    fn visit_float(&mut self, float: &f64) {
        todo!()
    }
}
