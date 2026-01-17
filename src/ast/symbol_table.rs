use std::collections::HashMap;

use crate::{ast::ASTBinaryOperatorKind, diagnostics::DiagnosticsColletionCell};

use super::{ASTReturnStatement, ASTStatementKind, ASTVisitor};

#[derive(Debug)]
struct VariableInfo {
    name: String,
    data_type: String,
}

#[derive(Debug)]
struct FunctionInfo {
    name: String,
    parameters: Vec<String>, // Simplified
    return_type: String,
}

enum Symbol {
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
struct FunctionContext {
    name: String,
    return_type: String,
}

#[derive(Debug)]
enum Pass {
    CollectSymbols,
    TypeCheck,
}

pub struct SymbolTable {
    global_scope: HashMap<String, Symbol>,
    scopes: Vec<HashMap<String, Symbol>>,
    pass: Pass,
    diagnostics: DiagnosticsColletionCell,
    function_stack: Vec<FunctionContext>,
}

impl SymbolTable {
    pub fn new(diagnostics: DiagnosticsColletionCell) -> Self {
        Self {
            global_scope: HashMap::new(),
            scopes: Vec::new(),
            pass: Pass::CollectSymbols,
            diagnostics,
            function_stack: Vec::new(),
        }
    }
    pub fn build(&mut self, ast: &super::Ast) {
        ast.visit(self);
        if !self.diagnostics.borrow().diagnostics.is_empty() {
            return;
        }
        self.pass = Pass::TypeCheck;
        ast.visit(self);
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_global_identifier(&mut self, symbol: Symbol) {
        if self.global_scope.contains_key(&symbol.name()) {
            // TODO(letohg): [2025-07-18] output diagnostic message
            // return Err(format!("Redefinition of global symbol `{}`", symbol.name));
        }

        self.global_scope.insert(symbol.name(), symbol);
    }
    fn declare_local_identifier(&mut self, symbol: Symbol) {
        let scope = self.scopes.last_mut().expect("No scope available");
        if scope.contains_key(&symbol.name()) {
            // TODO(letohg): [2025-07-18] output diagnostic message
            // return Err(format!("Redefinition of symbol `{}`", symbol.name));
        }
        scope.insert(symbol.name(), symbol);
    }

    fn is_identifier_in_current_scope(&self, name: &str) -> bool {
        let scope = self.scopes.last().expect("No scope available");
        scope.contains_key(name)
    }

    fn is_global_scope(&self) -> bool {
        return self.scopes.len() == 0;
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        self.global_scope.get(name)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum DataType {
    Void,
    Int,
    Float,
    Bool,
    Str,
    UserDefined(String),
}

impl DataType {
    fn from_token(token: &super::lexer::Token) -> Self {
        Self::from_string(&token.span.literal)
    }

    fn from_string(type_name: &String) -> Self {
        match type_name.as_str() {
            "void" => Self::Void,
            "i8" => Self::Int,
            "i16" => Self::Int,
            "i32" => Self::Int,
            "i64" => Self::Int,
            "u8" => Self::Int,
            "u16" => Self::Int,
            "u32" => Self::Int,
            "u64" => Self::Int,
            "f32" => Self::Float,
            "f64" => Self::Float,
            "bool" => Self::Bool,
            "str" => Self::Str,
            _ => Self::UserDefined(type_name.clone()),
        }
    }
    fn to_string(&self) -> String {
        match self {
            Self::Void => "void".to_string(),
            Self::Int => "i32".to_string(),
            Self::Float => "f32".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Str => "str".to_string(),
            Self::UserDefined(name) => name.clone(),
        }
    }
    fn get_common_type(type1: &Self, type2: &Self) -> Option<Self> {
        match (type1, type2) {
            (Self::Int, Self::Bool) => Some(Self::Int),
            (Self::Bool, Self::Int) => Some(Self::Int),
            (Self::Bool, Self::Float) => Some(Self::Float),
            (Self::Float, Self::Bool) => Some(Self::Float),
            (Self::Int, Self::Float) => Some(Self::Float),
            (Self::Float, Self::Int) => Some(Self::Float),
            (Self::Float, Self::UserDefined(_)) => None,
            (Self::Int, Self::UserDefined(_)) => None,
            (Self::Bool, Self::UserDefined(_)) => None,
            (Self::UserDefined(_), Self::Int) => None,
            (Self::UserDefined(_), Self::Float) => None,
            (Self::UserDefined(_), Self::Bool) => None,
            (Self::UserDefined(_), Self::UserDefined(_)) => None,
            (Self::Int, Self::Int) => Some(Self::Int),
            (Self::Float, Self::Float) => Some(Self::Float),
            (Self::Bool, Self::Bool) => Some(Self::Bool),
            (Self::Void, Self::Void) => Some(Self::Void),
            _ => todo!(),
        }
    }

    fn is_convertable_to(&self, wanted_type: Self) -> bool {
        if *self == wanted_type {
            return true;
        }
        match (self, wanted_type) {
            (Self::Int, Self::Int) => true,
            (Self::Int, Self::Float) => true,
            (Self::Int, Self::Bool) => true,
            (Self::Float, Self::Int) => true,
            (Self::Float, Self::Bool) => true,
            (Self::Bool, Self::Int) => true,
            (Self::Bool, Self::Float) => true,
            _ => false,
        }
    }
}

impl ASTVisitor<Option<DataType>> for SymbolTable {
    fn visit_return_statement(
        &mut self,
        statement: &super::ASTReturnStatement,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.diagnostics.borrow_mut().report_error(
                    format!("Return statement not allowed outside of functions"),
                    // TODO(letohg): [2026-01-16] diagnostic print does not work without the +1
                    super::lexer::TextSpan {
                        start: statement.keyword.span.start + 1,
                        end: statement.keyword.span.end,
                        literal: "return".to_string(),
                    },
                );
                None
            }
            Pass::TypeCheck => {
                let actual = self.visit_expression(&statement.expr)?;
                let expeceted =
                    DataType::from_string(&self.function_stack.last().unwrap().return_type);
                if actual != expeceted {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Expected return type of type {:?} but found {:?}",
                            expeceted, actual
                        ),
                        super::lexer::TextSpan {
                            start: statement.keyword.span.start,
                            end: statement.keyword.span.end,
                            literal: "return".to_string(),
                        },
                    );
                }
                Some(actual)
            }
        }
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.declare_global_identifier(Symbol::Constant(VariableInfo {
                    name: statement.identifier.name(),
                    data_type: statement.data_type.name(),
                }));
                None
            }
            Pass::TypeCheck => {
                if self.is_global_scope() {
                    return None;
                }
                if self.is_identifier_in_current_scope(&statement.identifier.name()) {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Redefinition of identifier!"),
                        statement.identifier.span.clone(),
                    );
                    return None;
                }

                let shadowed_identifier = self.lookup(&statement.identifier.name());
                match shadowed_identifier {
                    Some(Symbol::Function(_)) => {
                        self.diagnostics.borrow_mut().report_error(
                            format!("Identifier already used for function name"),
                            statement.identifier.span.clone(),
                        );
                        return None;
                    }
                    Some(Symbol::Variable(_)) | Some(Symbol::Constant(_)) => {
                        self.diagnostics.borrow_mut().report_warning(
                            format!("Declaration shadows identifier in outer scope"),
                            statement.identifier.span.clone(),
                        );
                        return None;
                    }
                    _ => {}
                };

                self.declare_local_identifier(Symbol::Constant(VariableInfo {
                    name: statement.identifier.name(),
                    data_type: statement.data_type.name(),
                }));
                let initialization_expr_type: DataType =
                    self.visit_expression(&statement.initializer)?;
                let actual = DataType::from_token(&statement.data_type);
                if initialization_expr_type.is_convertable_to(actual.clone()) {
                    // self.diagnostics.borrow_mut().report_warning(
                    //     format!(
                    //         "Implicit conversion from {:?} to {:?}",
                    //         initialization_expr_type, actual,
                    //     ),
                    //     statement.identifier.span.clone(),
                    // );
                } else {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Initializing an {:?} from a {:?}",
                            actual, initialization_expr_type
                        ),
                        statement.identifier.span.clone(),
                    );
                }

                None
            }
        }
    }

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.declare_global_identifier(Symbol::Variable(VariableInfo {
                    name: statement.identifier.name(),
                    data_type: statement.data_type.name(),
                }));
                None
            }
            Pass::TypeCheck => {
                if self.is_global_scope() {
                    return None;
                }

                let redefinition = self.lookup(statement.identifier.name().as_str());
                if !redefinition.is_none() {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Identifier {} already defined", statement.identifier.name()),
                        statement.identifier.span.clone(),
                    );
                }

                self.declare_local_identifier(Symbol::Variable(VariableInfo {
                    name: statement.identifier.name(),
                    data_type: statement.data_type.name(),
                }));
                let initialization_expr_type = self.visit_expression(&statement.initializer)?;
                let actual = DataType::from_token(&statement.data_type);
                if initialization_expr_type.is_convertable_to(actual.clone()) {
                    // self.diagnostics.borrow_mut().report_warning(
                    //     format!(
                    //         "Implicit conversion from {:?} to {:?}",
                    //         initialization_expr_type, actual,
                    //     ),
                    //     statement.identifier.span.clone(),
                    // );
                } else {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Initializing an {:?} from a {:?}",
                            actual, initialization_expr_type
                        ),
                        statement.identifier.span.clone(),
                    );
                }

                None
            }
        }
    }

    fn visit_compound_statement(
        &mut self,
        statement: &super::ASTCompoundStatement,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.diagnostics.borrow_mut().report_error(
                    format!("standalone Compound statement not allowed outside of functions"),
                    super::lexer::TextSpan {
                        start: statement.start_brace.span.start,
                        end: 0,
                        literal: "{".to_string(),
                    },
                );
                None
            }

            Pass::TypeCheck => {
                self.enter_scope();
                let mut first_return = None;
                for statement in statement.statements.iter() {
                    let return_type = self.visit_statement(statement);

                    match statement.kind {
                        ASTStatementKind::Return(_)
                        | ASTStatementKind::Compound(_)
                        | ASTStatementKind::For(_)
                        | ASTStatementKind::While(_)
                        | ASTStatementKind::If(_) => {
                            if let (Some(x), Some(y)) =
                                (first_return.as_ref(), return_type.as_ref())
                            {
                                if x != y {
                                    println!(
                                        "Return Type differs from previous return paths: {:?} {:?}",
                                        x, y
                                    );
                                }
                                // no assignment here
                            } else if first_return.is_none() {
                                first_return = return_type;
                            }
                        }
                        _ => {}
                    }
                }
                self.exit_scope();
                first_return
            }
        }
    }

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.diagnostics.borrow_mut().report_error(
                    format!("If statement not allowed outside of functions"),
                    statement.keyword.span.clone(),
                );
                None
            }
            Pass::TypeCheck => {
                let condition_type = self.visit_expression(&statement.condition)?;
                match condition_type {
                    DataType::Bool => {}
                    _ => {
                        self.diagnostics.borrow_mut().report_error(
                            format!("Condition must be of type Bool"),
                            statement.keyword.span.clone(),
                        );
                    }
                };

                let then_return_type = self.visit_statement(&statement.then_branch);

                if let Some(else_branch) = &statement.else_branch {
                    let else_return_type = self.visit_statement(&else_branch.else_branch);
                    if let (Some(trt), Some(ert)) =
                        (then_return_type.as_ref(), else_return_type.as_ref())
                    {
                        // if *ert == DataType::Void {
                        //     return then_return_type;
                        // }
                        // if *trt == DataType::Void {
                        //     return else_return_type;
                        // }

                        if *trt != *ert {
                            self.diagnostics.borrow_mut().report_error(
                                format!("Branches of If statement have different return types"),
                                statement.keyword.span.clone(),
                            );
                        }
                    }
                }
                // return then_return_type;
                None
            }
        }
    }

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.diagnostics.borrow_mut().report_error(
                    format!("For loop statement not allowed outside of functions"),
                    statement.keyword.span.clone(),
                );
                None
            }
            Pass::TypeCheck => {
                self.enter_scope();
                let range_start_type = self.visit_expression(&statement.range.0)?;
                let range_end_type = self.visit_expression(&statement.range.1)?;

                if range_start_type != range_end_type {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Range start and end condition have to have same type"),
                        statement.keyword.span.clone(),
                    );
                    return None;
                }

                self.declare_local_identifier(Symbol::Variable(VariableInfo {
                    name: statement.loop_variable.name(),
                    data_type: range_start_type.to_string(), // TODO(letohg): [2025-07-19] evaluate the
                                                             // datatype of statement.range (it has to be an integer)
                }));
                let return_type = self.visit_statement(&statement.body);
                self.exit_scope();
                // return_type
                None
            }
        }
    }

    fn visit_while_loop_statement(
        &mut self,
        statement: &super::ASTWhileStatement,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                self.diagnostics.borrow_mut().report_error(
                    format!("While statement not allowed outside of functions"),
                    statement.keyword.span.clone(),
                );
                None
            }
            Pass::TypeCheck => {
                let condition_type = self.visit_expression(&statement.condition)?;
                match condition_type {
                    DataType::Bool => {}
                    _ => {
                        self.diagnostics.borrow_mut().report_error(
                            format!("Condition must be of type Bool"),
                            statement.keyword.span.clone(),
                        );
                    }
                };

                self.visit_statement(&statement.body);
                None
            }
        }
    }

    fn visit_function_statement(
        &mut self,
        function: &super::ASTFunctionStatement,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => {
                let mut argument_types: Vec<String> = Vec::new();
                // add arguments to scope of local variable call
                for arg in function.arguments.iter() {
                    // argument_types.push(arg.identifier.span.literal.clone());
                    argument_types.push(arg.data_type.name());
                }
                self.declare_global_identifier(Symbol::Function(FunctionInfo {
                    name: function.identifier.name(),
                    parameters: argument_types.clone(),
                    return_type: function.return_type.span.literal.clone(),
                }));
                None
            }
            Pass::TypeCheck => {
                self.enter_scope();
                // add arguments to scope of local variable call
                for arg in function.arguments.iter() {
                    // argument_types.push(arg.identifier.span.literal.clone());
                    self.declare_local_identifier(Symbol::Variable(VariableInfo {
                        name: arg.identifier.name(),
                        data_type: arg.data_type.name(),
                    }));
                }
                self.function_stack.push(FunctionContext {
                    name: function.identifier.name(),
                    return_type: function.return_type.name(),
                });
                self.visit_statement(&function.body);
                self.function_stack.pop();
                self.exit_scope();
                None
            }
        }
    }

    fn visit_assignment_expression(
        &mut self,
        expr: &super::ASTAssignmentExpression,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => {
                let expr_data_type = self.visit_expression(&expr.expr)?;
                if let Some(identifier) = self.lookup(&expr.identifier.name().to_string()) {
                    match identifier {
                        Symbol::Function(_) => {
                            self.diagnostics.borrow_mut().report_error(
                                format!("Callables are not assignable {}", identifier.name()),
                                expr.identifier.span.clone(),
                            );
                            return None;
                        }
                        Symbol::Variable(v) => {
                            let expected = DataType::from_string(&v.data_type);

                            if expr_data_type.is_convertable_to(expected.clone()) {
                                // self.diagnostics.borrow_mut().report_warning(
                                //     format!(
                                //         "Implicit conversion from {:?} to {:?}",
                                //         expr_data_type, expected,
                                //     ),
                                //     expr.identifier.span.clone(),
                                // );
                            } else {
                                self.diagnostics.borrow_mut().report_error(
                                    format!(
                                        "Cannot assign {:?} to identifier '{}' of type {:?}",
                                        expr_data_type,
                                        identifier.name(),
                                        expected,
                                    ),
                                    expr.identifier.span.clone(),
                                );
                            }
                            // return Some(expected);
                            return None;
                        }
                        Symbol::Constant(c) => {
                            self.diagnostics.borrow_mut().report_error(
                                format!("Cannot reassign constant"),
                                expr.identifier.span.clone(),
                            );
                            return None;
                        }
                    };
                } else {
                    self.diagnostics
                        .borrow_mut()
                        .report_undefined_variable(expr.identifier.span.clone());
                    return None;
                }
            }
        }
    }

    fn visit_function_call_expression(
        &mut self,
        expr: &super::ASTFunctionCallExpression,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => {
                let mut func_return_type = DataType::Void;
                if expr.identifier() == "print" {
                } else if expr.identifier() == "println" {
                } else if self.lookup(&expr.identifier().to_string()).is_none() {
                    self.diagnostics
                        .borrow_mut()
                        .report_undefined_function(expr.identifier.span.clone());
                    return None;
                } else {
                    let expected_number_of_arguments = match self.lookup(expr.identifier()).unwrap()
                    {
                        Symbol::Function(func) => {
                            func_return_type = DataType::from_string(&func.return_type);
                            func.parameters.len()
                        }
                        _ => {
                            println!("Not a callable!");
                            return None;
                        }
                    };

                    if expected_number_of_arguments != expr.arguments.len() {
                        self.diagnostics
                            .borrow_mut()
                            .report_number_of_function_arguments_mismatch(
                                expr.identifier.span.clone(),
                                expected_number_of_arguments,
                                expr.arguments.len(),
                            );
                        return None;
                    }
                }
                for arg in expr.arguments.iter() {
                    self.visit_expression(arg);
                }
                Some(func_return_type)
            }
        }
    }

    fn visit_variable_expression(
        &mut self,
        expr: &super::ASTVariableExpression,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => {
                match self.lookup(&expr.identifier().to_string()) {
                    Some(var) => match var {
                        Symbol::Function(func) => {
                            self.diagnostics.borrow_mut().report_error(
                                format!(
                                    "Callable cannot be used as variable {}",
                                    expr.identifier()
                                ),
                                expr.identifier.span.clone(),
                            );
                        }
                        Symbol::Variable(v) | Symbol::Constant(v) => {
                            return Some(DataType::from_string(&v.data_type));
                        }
                    },
                    None => {
                        self.diagnostics
                            .borrow_mut()
                            .report_undefined_variable(expr.identifier.span.clone());
                    }
                };

                None
                // Some(DataType::Void)
            }
        }
    }

    fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => {
                let expr_data_type = self.visit_expression(&expr.expr)?;

                match expr.operator.kind {
                    super::ASTUnaryOperatorKind::Minus => {
                        if expr_data_type != DataType::Int && expr_data_type != DataType::Float {
                            self.diagnostics.borrow_mut().report_error(
                                format!(
                                    "Unary operator '-' can not be used on type {:?}",
                                    expr_data_type
                                ),
                                expr.operator.token.span.clone(),
                            );
                            return None;
                        }
                    }
                    super::ASTUnaryOperatorKind::BitwiseNOT => {
                        if expr_data_type != DataType::Int {
                            self.diagnostics.borrow_mut().report_error(
                                format!(
                                    "Unary operator '^' can not be used on type {:?}",
                                    expr_data_type
                                ),
                                expr.operator.token.span.clone(),
                            );
                            return None;
                        }
                    }
                    super::ASTUnaryOperatorKind::LogicNot => {
                        if expr_data_type != DataType::Bool {
                            self.diagnostics.borrow_mut().report_error(
                                format!(
                                    "Unary operator '!' can not be used on type {:?}",
                                    expr_data_type
                                ),
                                expr.operator.token.span.clone(),
                            );
                            return None;
                        }
                    }
                };
                return Some(expr_data_type);
            }
        }
    }

    fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => {
                let left_type = self.visit_expression(&expr.left)?;
                let right_type = self.visit_expression(&expr.right)?;

                let common_data_type = DataType::get_common_type(&left_type, &right_type);
                if common_data_type.is_none() {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "No common datatype between {:?} and {:?}",
                            left_type, right_type
                        ),
                        expr.operator.token.span.clone(),
                    );
                }

                match expr.operator.kind {
                    ASTBinaryOperatorKind::Plus
                    | ASTBinaryOperatorKind::Minus
                    | ASTBinaryOperatorKind::Multiply
                    | ASTBinaryOperatorKind::Divide => return common_data_type,
                    ASTBinaryOperatorKind::EqualTo
                    | ASTBinaryOperatorKind::NotEqualTo
                    | ASTBinaryOperatorKind::GreaterThan
                    | ASTBinaryOperatorKind::GreaterThanOrEqual
                    | ASTBinaryOperatorKind::LessThan
                    | ASTBinaryOperatorKind::LessThanOrEqual => return Some(DataType::Bool),
                    ASTBinaryOperatorKind::LogicAND | ASTBinaryOperatorKind::LogicOR => {
                        if left_type.is_convertable_to(DataType::Bool)
                            && right_type.is_convertable_to(DataType::Bool)
                        {
                            return Some(DataType::Bool);
                        }

                        self.diagnostics.borrow_mut().report_error(
                            format!(
                                "Both sides need to be convertable it to bool: {:?} and {:?} ",
                                left_type, right_type
                            ),
                            expr.operator.token.span.clone(),
                        );

                        return None;
                    }
                    ASTBinaryOperatorKind::BitwiseOR
                    | ASTBinaryOperatorKind::BitwiseAND
                    | ASTBinaryOperatorKind::BitwiseXOR => {
                        match common_data_type {
                            Some(DataType::Int) => return Some(DataType::Int),
                            _ => {}
                        };
                        self.diagnostics.borrow_mut().report_error(
                            format!(
                                "Both sides need to be of type int: {:?} and {:?} ",
                                left_type, right_type
                            ),
                            expr.operator.token.span.clone(),
                        );

                        return None;
                    }
                    _ => {}
                };
                return Some(right_type);
            }
        }
    }

    fn visit_parenthesised_expression(
        &mut self,
        expr: &super::ASTParenthesizedExpression,
    ) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => self.visit_expression(&expr.expr),
        }
    }

    fn visit_binary_operator(&mut self, op: &super::ASTBinaryOperator) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => None,
        }
    }
    fn visit_error(&mut self, span: &super::lexer::TextSpan) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => None,
        }
    }
    fn visit_integer(&mut self, integer: &i64) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => Some(DataType::Int),
        }
    }

    fn visit_boolean(&mut self, boolean: bool) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => Some(DataType::Bool),
        }
    }
    fn visit_string_literal(&mut self, string: &String) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => Some(DataType::Str),
        }
    }
    fn visit_float(&mut self, float: &f64) -> Option<DataType> {
        match self.pass {
            Pass::CollectSymbols => None,
            Pass::TypeCheck => Some(DataType::Float),
        }
    }
}
