use crate::ast::symbol_table::DataType;
use crate::ast::symbol_table::FunctionContext;
use crate::ast::symbol_table::Symbol;
use crate::ast::symbol_table::SymbolTable;
use crate::ast::symbol_table::VariableInfo;
use crate::{ast::ASTBinaryOperatorKind, diagnostics::DiagnosticsColletionCell};

use super::{ASTStatementKind, ASTVisitor};

pub struct TypeChecker<'a> {
    symbol_table: &'a mut SymbolTable,
    diagnostics: DiagnosticsColletionCell,
}

impl<'a> TypeChecker<'a> {
    pub fn new(diagnostics: DiagnosticsColletionCell, symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            diagnostics,
        }
    }

    pub fn analyze(&mut self, ast: &super::Ast) {
        ast.visit(self);
    }

    fn enter_scope(&mut self) {
        self.symbol_table.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.symbol_table.exit_scope();
    }

    fn declare_global_identifier(&mut self, symbol: Symbol) {
        self.symbol_table.declare_global_identifier(symbol)
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

impl<'a> ASTVisitor<Option<DataType>> for TypeChecker<'a> {
    fn visit_return_statement(
        &mut self,
        statement: &super::ASTReturnStatement,
    ) -> Option<DataType> {
        let actual = self.visit_expression(&statement.expr)?;
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

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) -> Option<DataType> {
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
            data_type: DataType::from_token(&statement.data_type),
        }));
        let initialization_expr_type: DataType = self.visit_expression(&statement.initializer)?;
        let actual = DataType::from_token(&statement.data_type);
        if initialization_expr_type.is_convertable_to(actual.clone()) {
            // self.diagnostics.borrow_mut().report_warning(
            //     format!(
            //         "Implicit conversion from {} to {}",
            //         initialization_expr_type, actual,
            //     ),
            //     statement.identifier.span.clone(),
            // );
        } else {
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

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) -> Option<DataType> {
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
            data_type: DataType::from_token(&statement.data_type),
        }));
        let initialization_expr_type = self.visit_expression(&statement.initializer)?;
        let actual = DataType::from_token(&statement.data_type);
        if initialization_expr_type.is_convertable_to(actual.clone()) {
            // self.diagnostics.borrow_mut().report_warning(
            //     format!(
            //         "Implicit conversion from {} to {}",
            //         initialization_expr_type, actual,
            //     ),
            //     statement.identifier.span.clone(),
            // );
        } else {
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
        statement: &super::ASTCompoundStatement,
    ) -> Option<DataType> {
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
                    if let (Some(x), Some(y)) = (first_return.as_ref(), return_type.as_ref()) {
                        if x != y {
                            println!(
                                "Return Type differs from previous return paths: {} {}",
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

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) -> Option<DataType> {
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

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) -> Option<DataType> {
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
            data_type: range_start_type, // TODO(letohg): [2025-07-19] evaluate the
                                         // datatype of statement.range (it has to be an integer)
        }));
        let _return_type = self.visit_statement(&statement.body);
        self.exit_scope();
        // return_type
        None
    }

    fn visit_while_loop_statement(
        &mut self,
        statement: &super::ASTWhileStatement,
    ) -> Option<DataType> {
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

    fn visit_function_statement(
        &mut self,
        function: &super::ASTFunctionStatement,
    ) -> Option<DataType> {
        self.enter_scope();
        // add arguments to scope of local variable call
        for arg in function.arguments.iter() {
            // argument_types.push(arg.identifier.span.literal.clone());
            self.declare_local_identifier(Symbol::Variable(VariableInfo {
                name: arg.identifier.name(),
                data_type: DataType::from_token(&arg.data_type),
            }));
        }
        self.symbol_table.function_stack.push(FunctionContext {
            name: function.identifier.name(),
            return_type: DataType::from_token(&function.return_type),
        });
        self.visit_statement(&function.body);
        self.symbol_table.function_stack.pop();
        self.exit_scope();
        None
    }

    fn visit_struct_statement(
        &mut self,
        struct_def: &super::ASTStructStatement,
    ) -> Option<DataType> {
        Some(DataType::from_token(&struct_def.identifier))
    }

    fn visit_assignment_expression(
        &mut self,
        expr: &super::ASTAssignmentExpression,
    ) -> Option<DataType> {
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
                Symbol::Variable(VariableInfo {
                    name: _,
                    data_type: expected,
                }) => {
                    if expr_data_type.is_convertable_to(expected.clone()) {
                        // self.diagnostics.borrow_mut().report_warning(
                        //     format!(
                        //         "Implicit conversion from {} to {}",
                        //         expr_data_type, expected,
                        //     ),
                        //     expr.identifier.span.clone(),
                        // );
                    } else {
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
        expr: &super::ASTFunctionCallExpression,
    ) -> Option<DataType> {
        let mut func_return_type = DataType::Void;
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
            self.visit_expression(arg);
        }
        Some(func_return_type)
    }

    fn visit_variable_expression(
        &mut self,
        expr: &super::ASTVariableExpression,
    ) -> Option<DataType> {
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

    fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) -> Option<DataType> {
        let expr_data_type = self.visit_expression(&expr.expr)?;

        match expr.operator.kind {
            super::ASTUnaryOperatorKind::Minus => {
                if expr_data_type.is_integer() && expr_data_type.is_floating() {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Unary operator '-' can not be used on type {}",
                            expr_data_type
                        ),
                        expr.operator.token.span.clone(),
                    );
                    return None;
                }
            }
            super::ASTUnaryOperatorKind::BitwiseNOT => {
                if expr_data_type.is_integer() {
                    self.diagnostics.borrow_mut().report_error(
                        format!(
                            "Unary operator '^' can not be used on type {}",
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
                            "Unary operator '!' can not be used on type {}",
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

    fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) -> Option<DataType> {
        let left_type = self.visit_expression(&expr.left)?;
        let right_type = self.visit_expression(&expr.right)?;

        let common_data_type = DataType::get_common_type(&left_type, &right_type);
        if common_data_type.is_none() {
            self.diagnostics.borrow_mut().report_error(
                format!(
                    "No common datatype between {} and {}",
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
                        "Both sides need to be convertable it to bool: {} and {}",
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
                    Some(ty) => {
                        if ty.is_integer() {
                            return Some(ty);
                        }
                    }
                    _ => {}
                };
                self.diagnostics.borrow_mut().report_error(
                    format!(
                        "Both sides need to be of type int: {} and {}",
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

    fn visit_parenthesised_expression(
        &mut self,
        expr: &super::ASTParenthesizedExpression,
    ) -> Option<DataType> {
        self.visit_expression(&expr.expr)
    }
    fn visit_binary_operator(&mut self, _op: &super::ASTBinaryOperator) -> Option<DataType> {
        None
    }
    fn visit_error(&mut self, _span: &super::lexer::TextSpan) -> Option<DataType> {
        None
    }
    fn visit_integer(&mut self, _integer: &i64) -> Option<DataType> {
        Some(DataType::I32)
    }
    fn visit_boolean(&mut self, _boolean: bool) -> Option<DataType> {
        Some(DataType::Bool)
    }
    fn visit_float(&mut self, _float: &f64) -> Option<DataType> {
        Some(DataType::F32)
    }
}
