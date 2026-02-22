use crate::ast::symbol_table::FunctionContext;
use crate::ast::symbol_table::Symbol;
use crate::ast::symbol_table::SymbolTable;
use crate::ast::symbol_table::VariableInfo;
use crate::ast::typing::TypeId;
use crate::ast::typing::TypeTable;
use crate::ast::Ast;
use crate::{ast::ASTBinaryOperatorKind, diagnostics::DiagnosticsCollectionCell};

use super::{ASTStatementKind, ASTVisitor};

pub struct TypeChecker<'a> {
    symbol_table: &'a mut SymbolTable,
    diagnostics: DiagnosticsCollectionCell,
    type_table: &'a mut TypeTable,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        diagnostics: DiagnosticsCollectionCell,
        symbol_table: &'a mut SymbolTable,
        type_table: &'a mut TypeTable,
    ) -> Self {
        Self {
            symbol_table,
            diagnostics,
            type_table,
        }
    }

    pub fn analyze(&mut self, ast: &mut super::Ast) {
        for id in ast.top_level_statements.clone().iter() {
            self.visit_statement(ast, *id);
        }
    }

    fn enter_scope(&mut self) {
        self.symbol_table.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.symbol_table.exit_scope();
    }

    fn declare_local_identifier(&mut self, symbol: Symbol) {
        self.symbol_table.declare_local_identifier(symbol)
    }

    fn is_identifier_in_current_scope(&self, name: &str) -> bool {
        self.symbol_table.is_identifier_in_current_scope(name)
    }

    fn is_global_scope(&self) -> bool {
        self.symbol_table.is_global_scope()
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbol_table.lookup(name)
    }
}

impl<'a> ASTVisitor<Option<TypeId>> for TypeChecker<'a> {
    fn visit_return_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTReturnStatement,
    ) -> Option<TypeId> {
        let actual = self.visit_expression(ast, statement.expr)?;
        let expeceted = &self.symbol_table.function_stack.last().unwrap().return_type;
        if actual != *expeceted {
            self.diagnostics.borrow_mut().report_error(
                format!(
                    "Expected return type of type {} but found {}",
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

    fn visit_let_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTLetStatement,
    ) -> Option<TypeId> {
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
            data_type: self
                .type_table
                .get_builtin_from_token(&statement.data_type)
                .unwrap(),
        }));
        let initialization_expr_type: TypeId = self.visit_expression(ast, statement.initializer)?;
        let actual = self
            .type_table
            .get_builtin_from_token(&statement.data_type)
            .unwrap();
        if initialization_expr_type != actual {
            self.diagnostics.borrow_mut().report_error(
                format!(
                    "Initializing an {} from a {}",
                    actual, initialization_expr_type
                ),
                statement.identifier.span.clone(),
            );
        }

        None
    }

    fn visit_var_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTVarStatement,
    ) -> Option<TypeId> {
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
            data_type: self
                .type_table
                .get_builtin_from_token(&statement.data_type)
                .unwrap(),
        }));
        let initialization_expr_type: TypeId = self.visit_expression(ast, statement.initializer)?;
        let actual = self
            .type_table
            .get_builtin_from_token(&statement.data_type)
            .unwrap();
        if initialization_expr_type != actual {
            self.diagnostics.borrow_mut().report_error(
                format!(
                    "Initializing an {} from a {}",
                    actual, initialization_expr_type
                ),
                statement.identifier.span.clone(),
            );
        }

        None
    }

    fn visit_compound_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTCompoundStatement,
    ) -> Option<TypeId> {
        self.enter_scope();
        let mut first_return: Option<TypeId> = None;
        for stmnt in statement.statements.iter() {
            let return_type: Option<TypeId> = self.visit_statement(ast, *stmnt);

            matches!(
                ast.query_statement(*stmnt).kind,
                ASTStatementKind::Return(_)
                    | ASTStatementKind::Compound(_)
                    | ASTStatementKind::For(_)
                    | ASTStatementKind::While(_)
                    | ASTStatementKind::If(_)
            )
            .then(|| {
                if let (Some(x), Some(y)) = (first_return.as_ref(), return_type.as_ref()) {
                    if x != y {
                        self.diagnostics.borrow_mut().report_error(
                            format!(
                                "Return Type differs from previous return paths: {} and {}",
                                x, y
                            ),
                            // ast.query_statement(*stmnt).span.clone(),
                            super::lexer::TextSpan {
                                start: 0,
                                end: 0,
                                literal: "".to_string(),
                            },
                        );
                    }
                    // no assignment here
                } else if first_return.is_none() {
                    first_return = return_type;
                }
            });
        }
        self.exit_scope();
        first_return
    }

    fn visit_if_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTIfStatement,
    ) -> Option<TypeId> {
        let condition_type = self.visit_expression(ast, statement.condition)?;
        if !self.type_table.is_boolean(condition_type) {
            self.diagnostics.borrow_mut().report_error(
                format!("Condition must be of type Bool"),
                statement.keyword.span.clone(),
            );
        }

        let then_return_type = self.visit_statement(ast, statement.then_branch);

        if let Some(else_branch) = &statement.else_branch {
            let else_return_type = self.visit_statement(ast, else_branch.else_branch);
            if let (Some(trt), Some(ert)) = (then_return_type.as_ref(), else_return_type.as_ref()) {
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

    fn visit_for_loop_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTForStatement,
    ) -> Option<TypeId> {
        self.enter_scope();
        let range_start_type = self.visit_expression(ast, statement.range.0)?;
        let range_end_type = self.visit_expression(ast, statement.range.1)?;

        if range_start_type != range_end_type {
            self.diagnostics.borrow_mut().report_error(
                format!("Range start and end condition have to have same type"),
                statement.keyword.span.clone(),
            );
            return None;
        }

        self.declare_local_identifier(Symbol::Variable(VariableInfo {
            name: statement.loop_variable.name(),
            data_type: range_start_type, // TODO(letohg): [2025-07-19] evaluate the
                                         // datatype of statement.range (it has to be an integer)
        }));
        let _return_type = self.visit_statement(ast, statement.body);
        self.exit_scope();
        // return_type
        None
    }

    fn visit_while_loop_statement(
        &mut self,
        ast: &mut Ast,
        statement: &super::ASTWhileStatement,
    ) -> Option<TypeId> {
        let condition_type = self.visit_expression(ast, statement.condition)?;
        if self.type_table.is_boolean(condition_type) {
            self.diagnostics.borrow_mut().report_error(
                format!("Condition must be of type Bool"),
                statement.keyword.span.clone(),
            );
        }

        self.visit_statement(ast, statement.body);
        None
    }

    fn visit_function_statement(
        &mut self,
        ast: &mut Ast,
        function: &super::ASTFunctionStatement,
    ) -> Option<TypeId> {
        self.enter_scope();
        // add arguments to scope of local variable call
        for arg in function.arguments.iter() {
            // argument_types.push(arg.identifier.span.literal.clone());
            self.declare_local_identifier(Symbol::Variable(VariableInfo {
                name: arg.identifier.name(),
                data_type: self.type_table.get_builtin_from_token(&arg.data_type)?,
            }));
        }
        self.symbol_table.function_stack.push(FunctionContext {
            name: function.identifier.name(),
            return_type: self
                .type_table
                .get_builtin_from_token(&function.return_type)?,
        });
        self.visit_statement(ast, function.body);
        self.symbol_table.function_stack.pop();
        self.exit_scope();
        None
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast,
        expr: &super::ASTAssignmentExpression,
        _expr: &super::ASTExpression,
    ) -> Option<TypeId> {
        let expr_data_type = self.visit_expression(ast, expr.expr)?;
        if let Some(identifier) = self.lookup(&expr.identifier.name().to_string()) {
            match identifier {
                Symbol::Function(_) => {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Callables are not assignable {}", identifier.name()),
                        expr.identifier.span.clone(),
                    );
                    return None;
                }
                Symbol::Variable(VariableInfo {
                    name: _,
                    data_type: expected,
                }) => {
                    if expr_data_type != *expected {
                        self.diagnostics.borrow_mut().report_error(
                            format!(
                                "Cannot assign {} to identifier '{}' of type {}",
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
                Symbol::Constant(_) => {
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

    fn visit_function_call_expression(
        &mut self,
        ast: &mut Ast,
        expr: &super::ASTFunctionCallExpression,
        _expr: &super::ASTExpression,
    ) -> Option<TypeId> {
        let mut func_return_type = self
            .type_table
            .get_builtin(super::typing::BuiltinType::Void)
            .unwrap();
        if expr.identifier() == "println" {
        } else if self.lookup(&expr.identifier().to_string()).is_none() {
            self.diagnostics
                .borrow_mut()
                .report_undefined_function(expr.identifier.span.clone());
            return None;
        } else {
            let expected_number_of_arguments = match self.lookup(expr.identifier()).unwrap() {
                Symbol::Function(func) => {
                    func_return_type = func.return_type.clone();
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
            self.visit_expression(ast, *arg);
        }
        Some(func_return_type)
    }

    fn visit_variable_expression(
        &mut self,
        ast: &mut Ast,
        expr: &super::ASTVariableExpression,
        _expr: &super::ASTExpression,
    ) -> Option<TypeId> {
        match self.lookup(&expr.identifier().to_string()) {
            Some(var) => match var {
                Symbol::Function(_func) => {
                    self.diagnostics.borrow_mut().report_error(
                        format!("Callable cannot be used as variable {}", expr.identifier()),
                        expr.identifier.span.clone(),
                    );
                }
                Symbol::Variable(v) | Symbol::Constant(v) => {
                    return Some(v.data_type.clone());
                }
            },
            _ => {
                self.diagnostics
                    .borrow_mut()
                    .report_undefined_variable(expr.identifier.span.clone());
            }
        };

        None
        // Some(DataType::Void)
    }

    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast,
        unary_expr: &super::ASTUnaryExpression,
        expr: &super::ASTExpression,
    ) -> Option<TypeId> {
        let expr_data_type = self.visit_expression(ast, unary_expr.expr)?;

        match unary_expr.operator.kind {
            super::ASTUnaryOperatorKind::Minus => {
                if !self.type_table.is_numeric(expr_data_type) {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Unary operator '-' can not be used on type {}",
                            expr_data_type
                        ),
                        unary_expr.operator.token.span.clone(),
                    );
                    return None;
                }
            }
            super::ASTUnaryOperatorKind::BitwiseNOT => {
                if !self.type_table.is_integer(expr_data_type) {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Unary operator '^' can not be used on type {}",
                            expr_data_type
                        ),
                        unary_expr.operator.token.span.clone(),
                    );
                    return None;
                }
            }
            super::ASTUnaryOperatorKind::LogicNot => {
                if !self.type_table.is_boolean(expr_data_type) {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Unary operator '!' can not be used on type {}",
                            expr_data_type
                        ),
                        unary_expr.operator.token.span.clone(),
                    );
                    return None;
                }
            }
        };
        ast.query_expression_mut(expr.id).ty = Some(expr_data_type);
        return Some(expr_data_type);
    }

    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast,
        bin_expr: &super::ASTBinaryExpression,
        expr: &super::ASTExpression,
    ) -> Option<TypeId> {
        let left_type = self.visit_expression(ast, bin_expr.left)?;
        let right_type = self.visit_expression(ast, bin_expr.right)?;

        // let common_data_type = DataType::get_common_type(&left_type, &right_type);
        // if common_data_type.is_none() {
        if left_type != right_type {
            self.diagnostics.borrow_mut().report_error(
                format!(
                    "No common datatype between {} and {}",
                    left_type, right_type
                ),
                bin_expr.operator.token.span.clone(),
            );
        }

        match bin_expr.operator.kind {
            ASTBinaryOperatorKind::Plus
            | ASTBinaryOperatorKind::Minus
            | ASTBinaryOperatorKind::Multiply
            | ASTBinaryOperatorKind::Divide => return Some(left_type), // TODO(letohg): [2025-07-19] evaluate the common type of left and right and return it
            ASTBinaryOperatorKind::EqualTo
            | ASTBinaryOperatorKind::NotEqualTo
            | ASTBinaryOperatorKind::GreaterThan
            | ASTBinaryOperatorKind::GreaterThanOrEqual
            | ASTBinaryOperatorKind::LessThan
            | ASTBinaryOperatorKind::LessThanOrEqual => {
                let type_id = self
                    .type_table
                    .get_builtin(super::typing::BuiltinType::Bool);
                ast.query_expression_mut(expr.id).ty = type_id;
                return type_id;
            }
            ASTBinaryOperatorKind::LogicAND | ASTBinaryOperatorKind::LogicOR => {
                if self.type_table.is_boolean(left_type) && self.type_table.is_boolean(right_type) {
                    let type_id = self
                        .type_table
                        .get_builtin(super::typing::BuiltinType::Bool);
                    ast.query_expression_mut(expr.id).ty = type_id;
                    return type_id;
                }
                self.diagnostics.borrow_mut().report_error(
                    format!(
                        "Both sides need to be convertable it to bool: {} and {}",
                        left_type, right_type
                    ),
                    bin_expr.operator.token.span.clone(),
                );

                return None;
            }
            ASTBinaryOperatorKind::BitwiseOR
            | ASTBinaryOperatorKind::BitwiseAND
            | ASTBinaryOperatorKind::BitwiseXOR => {
                // TODO(letohg): [2025-07-19] evaluate the common type of left and right and return it
                if self.type_table.is_integer(left_type) && self.type_table.is_integer(right_type) {
                    ast.query_expression_mut(expr.id).ty = Some(left_type);
                    return Some(left_type);
                }
                self.diagnostics.borrow_mut().report_error(
                    format!(
                        "Both sides need to be of type int: {} and {}",
                        left_type, right_type
                    ),
                    bin_expr.operator.token.span.clone(),
                );

                return None;
            }
        };
    }

    fn visit_parenthesised_expression(
        &mut self,
        ast: &mut Ast,
        paren_expr: &super::ASTParenthesizedExpression,
        expr: &super::ASTExpression,
    ) -> Option<TypeId> {
        let type_id = self.visit_expression(ast, paren_expr.expr);
        ast.query_expression_mut(expr.id).ty = type_id;
        return type_id;
    }
    fn visit_binary_operator(&mut self, _op: &super::ASTBinaryOperator) -> Option<TypeId> {
        None
    }
    fn visit_error(&mut self, _span: &super::lexer::TextSpan) -> Option<TypeId> {
        None
    }
    fn visit_integer(&mut self, _integer: &i64, _expr: &super::ASTExpression) -> Option<TypeId> {
        self.type_table.get_builtin(super::typing::BuiltinType::I32)
    }
    fn visit_boolean(&mut self, _boolean: bool, _expr: &super::ASTExpression) -> Option<TypeId> {
        self.type_table
            .get_builtin(super::typing::BuiltinType::Bool)
    }
    fn visit_float(&mut self, _float: &f64, _expr: &super::ASTExpression) -> Option<TypeId> {
        self.type_table.get_builtin(super::typing::BuiltinType::F32)
    }
}
