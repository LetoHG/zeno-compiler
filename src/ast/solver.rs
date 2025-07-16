use std::{collections::HashMap, ops::Not};

use super::{
    lexer::{TextSpan, TokenKind},
    ASTBinaryOperator, ASTBinaryOperatorKind, ASTFunctionStatement, ASTReturnStatement, ASTVisitor,
};

type Scope = HashMap<String, f64>;
pub struct ASTSolver {
    result: Option<f64>,
    scopes: Vec<Scope>,
    functions: HashMap<String, ASTFunctionStatement>,
}

impl ASTSolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            result: None,
            functions: HashMap::new(),
        }
    }

    pub fn solve(&mut self) {
        if let Some(entry_point) = self.functions.get("main") {
            self.result = None;
            self.visit_function_call_expression(&super::ASTFunctionCallExpression {
                identifier: super::lexer::Token {
                    kind: TokenKind::Identifier,
                    span: TextSpan {
                        start: 0,
                        end: 0,
                        literal: "main".to_string(),
                    },
                },
                arguments: vec![],
            });
            self.print_result();
        } else {
            println!("No entry point to program. `func main()` is missing");
        }
    }

    pub fn print_result(&self) {
        println!("Solver result: {}", self.result.unwrap());
    }

    fn enter_scope(&mut self, scope_variables: Scope) {
        self.scopes.push(scope_variables);
        // self.active_scope += 1;
    }

    fn leave_scope(&mut self) {
        self.scopes.pop();
        // self.active_scope -= 1;
    }

    fn add_identifier_to_scope(&mut self, identifier: &String, value: f64) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(identifier.clone(), value);
    }

    fn check_identifier_in_scope(&self, identifier: &String) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(identifier) {
                return true;
            }
        }
        return false;
    }

    fn get_identifier_in_scope(&self, identifier: &String) -> Option<f64> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(identifier) {
                return scope.get(identifier).copied();
            }
        }
        return None;
    }
}

impl ASTVisitor<Option<()>> for ASTSolver {
    fn visit_return_statement(&mut self, statement: &ASTReturnStatement) -> Option<()> {
        self.visit_expression(&statement.expr);
        Some(())
    }

    fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) -> Option<()> {
        for statement in statement.statements.iter() {
            match statement.kind {
                super::ASTStatementKind::Return(_) => return self.visit_statement(statement),
                _ => {
                    let ret = self.visit_statement(statement);
                    if ret.is_some() {
                        return ret;
                    }
                }
            };
        }
        None
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) -> Option<()> {
        self.visit_expression(&statement.initializer);
        self.add_identifier_to_scope(&statement.identifier.span.literal, self.result.unwrap());
        None
    }

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) -> Option<()> {
        self.visit_expression(&statement.initializer);
        self.add_identifier_to_scope(&statement.identifier.span.literal, self.result.unwrap());
        None
    }

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) -> Option<()> {
        self.visit_expression(&statement.condition);
        let condition = self.result.unwrap();

        if condition != 0.0 {
            return self.visit_statement(&statement.then_branch);
        } else if let Some(else_branch) = &statement.else_branch {
            return self.visit_statement(&else_branch.else_branch);
        }
        None
    }

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) -> Option<()> {
        self.visit_expression(&statement.range.0);
        let range_start = self.result.unwrap() as i64;
        self.visit_expression(&statement.range.1);
        let range_end = self.result.unwrap() as i64 + 1;
        let loop_var = statement.loop_variable.span.literal.clone();

        self.enter_scope(Scope::new());
        self.add_identifier_to_scope(&loop_var, range_start as f64);

        for i in range_start..range_end {
            let ret = self.visit_statement(&statement.body);
            for scope in self.scopes.iter_mut().rev() {
                if let Some(value) = scope.get_mut(&loop_var) {
                    *value = i as f64;
                    break;
                }
            }
            if ret.is_some() {
                self.leave_scope();
                return ret;
            }
        }
        self.leave_scope();
        None
    }

    fn visit_while_loop_statement(&mut self, statement: &super::ASTWhileStatement) -> Option<()> {
        self.enter_scope(Scope::new());
        loop {
            self.visit_expression(&statement.condition);
            let condition = self.result.unwrap() as i64 != 0;
            if !condition {
                break;
            }

            let ret = self.visit_statement(&statement.body);
            if ret.is_some() {
                self.leave_scope();
                return ret;
            }
        }
        self.leave_scope();
        None
    }

    fn visit_function_statement(&mut self, function: &super::ASTFunctionStatement) -> Option<()> {
        self.functions
            .insert(function.identifier.span.literal.clone(), function.clone());

        self.add_identifier_to_scope(&function.identifier.span.literal, 0.0);
        None
    }

    fn visit_assignment_expression(&mut self, expr: &super::ASTAssignmentExpression) -> Option<()> {
        self.visit_expression(&expr.expr);
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(&expr.identifier.span.literal) {
                *value = self.result.unwrap();
                break;
            }
        }
        None
    }

    fn visit_function_call_expression(
        &mut self,
        expr: &super::ASTFunctionCallExpression,
    ) -> Option<()> {
        // TODO(letohg): [2025-07-19] support for builtin functions
        if expr.identifier() == "println" {
            for arg_expr in expr.arguments.iter() {
                self.visit_expression(&arg_expr);
                // let arg_name = func_arg.identifier.span.literal.clone();

                println!("println: {}", self.result.unwrap());
            }
            if expr.arguments.len() == 0 {
                println!("");
            }
            return None;
        }

        if !self.check_identifier_in_scope(&expr.identifier.span.literal) {}

        let func = self
            .functions
            .get(&expr.identifier.span.literal)
            .unwrap()
            .clone();
        let mut arguments: Scope = Scope::new();

        // evaluate arguments and add them to scope
        // arguments.push(expr.identifier.span.literal.clone());
        for (arg_expr, func_arg) in expr.arguments.iter().zip(func.arguments.iter()) {
            self.visit_expression(&arg_expr);
            let arg_name = func_arg.identifier.span.literal.clone();

            arguments.insert(arg_name, self.result.unwrap());
        }
        self.enter_scope(arguments);

        // todo: that check should be done before
        if let super::ASTStatementKind::Compound(statement) = &func.body.kind {
            for statement in statement.statements.iter() {
                let result = self.visit_statement(statement);
                if result.is_some() {
                    return result;
                }
            }
        }

        self.leave_scope();
        None
    }

    fn visit_variable_expression(&mut self, expr: &super::ASTVariableExpression) -> Option<()> {
        self.result = self.get_identifier_in_scope(&expr.identifier.span.literal);
        // self.result = Some(*self.variables.get(expr.identifier()).unwrap());
        None
    }

    fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) -> Option<()> {
        self.visit_expression(&expr.expr);
        self.result = Some(match expr.operator.kind {
            super::ASTUnaryOperatorKind::BitwiseNOT => (self.result.unwrap() as i64).not() as f64,
            super::ASTUnaryOperatorKind::LogicNot => ((self.result.unwrap() == 0.0) as i64) as f64,
            super::ASTUnaryOperatorKind::Minus => self.result.unwrap() * -1.0,
        });
        None
    }
    fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) -> Option<()> {
        self.visit_expression(&expr.left);
        let left = self.result.unwrap();
        self.visit_expression(&expr.right);
        let right = self.result.unwrap();
        self.result = Some(match expr.operator.kind {
            ASTBinaryOperatorKind::Plus => left + right,
            ASTBinaryOperatorKind::Minus => left - right,
            ASTBinaryOperatorKind::Multiply => left * right,
            ASTBinaryOperatorKind::Divide => left / right,
            ASTBinaryOperatorKind::EqualTo => (left == right) as i64 as f64,
            ASTBinaryOperatorKind::NotEqualTo => (left != right) as i64 as f64,
            ASTBinaryOperatorKind::LogicAND => ((left != 0.0) && (right != 0.0)) as i64 as f64,
            ASTBinaryOperatorKind::LogicOR => ((left != 0.0) || (right != 0.0)) as i64 as f64,
            ASTBinaryOperatorKind::GreaterThan => (left > right) as i64 as f64,
            ASTBinaryOperatorKind::GreaterThanOrEqual => (left >= right) as i64 as f64,
            ASTBinaryOperatorKind::LessThan => (left < right) as i64 as f64,
            ASTBinaryOperatorKind::LessThanOrEqual => (left <= right) as i64 as f64,
            ASTBinaryOperatorKind::BitwiseOR => ((left as i64) | (right as i64)) as f64,
            ASTBinaryOperatorKind::BitwiseAND => ((left as i64) & (right as i64)) as f64,
            ASTBinaryOperatorKind::BitwiseXOR => ((left as i64) ^ (right as i64)) as f64,
        });
        None
    }

    fn visit_parenthesised_expression(
        &mut self,
        expr: &super::ASTParenthesizedExpression,
    ) -> Option<()> {
        return self.visit_expression(&expr.expr);
    }

    fn visit_binary_operator(&mut self, op: &ASTBinaryOperator) -> Option<()> {
        None
    }

    fn visit_error(&mut self, span: &super::lexer::TextSpan) -> Option<()> {
        None
    }

    fn visit_integer(&mut self, integer: &i64) -> Option<()> {
        self.result = Some(integer.clone() as f64);
        None
    }
    fn visit_boolean(&mut self, boolean: bool) -> Option<()> {
        self.result = Some(boolean as i64 as f64);
        None
    }
    fn visit_float(&mut self, float: &f64) -> Option<()> {
        self.result = Some(float.clone());
        None
    }
}
