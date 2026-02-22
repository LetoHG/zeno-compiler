use std::collections::HashMap;

use crate::diagnostics::DiagnosticsColletionCell;

use super::ASTVisitor;

pub struct TypeChecker<'a> {
    symbol_table: &'a SymbolTable,
    diagnostics: DiagnosticsColletionCell,
}

impl<'a> TypeChecker<'a> {
    pub fn new(diagnostics: DiagnosticsColletionCell, symbol_tabel: &mut SymbolTable) -> Self {
        Self {
            symbol_table,
            diagnostics,
        }
    }

    fn get_active_scope(&self) -> &Vec<String> {
        self.scopes.get(self.active_scope).unwrap()
    }

    fn enter_scope(&mut self, scope_variables: Vec<String>) {
        self.scopes.push(scope_variables);
        self.active_scope += 1;
    }

    fn leave_scope(&mut self) {
        self.scopes.pop();
        self.active_scope -= 1;
    }

    fn add_identifier_to_scope(&mut self, identifier: &String) {
        self.scopes.last_mut().unwrap().push(identifier.clone());
    }

    fn check_identifier_in_scope(&self, identifier: &String) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.contains(identifier) {
                return true;
            }
        }
        return false;
    }
}

impl ASTVisitor<()> for TypeChecker {
    fn visit_return_statement(&mut self, ast: &mut Ast, statement: &super::ASTReturnStatement) {
        self.visit_expression(&statement.expr);
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) {
        self.add_identifier_to_scope(&statement.identifier.span.literal);
        self.visit_expression(&statement.initializer);
    }

    fn visit_var_statement(&mut self, ast: &mut Ast, statement: &super::ASTVarStatement) {
        self.add_identifier_to_scope(&statement.identifier.span.literal);
        self.visit_expression(&statement.initializer);
    }

    fn visit_compound_statement(&mut self, ast: &mut Ast, statement: &super::ASTCompoundStatement) {
        self.enter_scope(Vec::new());
        for statement in statement.statements.iter() {
            self.visit_statement(statement);
        }
        self.leave_scope();
    }

    fn visit_if_statement(&mut self, ast: &mut Ast, statement: &super::ASTIfStatement) {
        self.visit_statement(&statement.then_branch);
        if let Some(else_branch) = &statement.else_branch {
            self.visit_statement(&else_branch.else_branch);
        }
    }

    fn visit_for_loop_statement(&mut self, ast: &mut Ast, statement: &super::ASTForStatement) {}

    fn visit_while_loop_statement(&mut self, ast: &mut Ast, statement: &super::ASTWhileStatement) {}

    fn visit_function_statement(&mut self, ast: &mut Ast, function: &super::ASTFunctionStatement) {
        self.add_identifier_to_scope(&function.identifier.span.literal);

        let mut arguments_names: Vec<String> = Vec::new();
        // add arguments to scope of local variable call
        for arg in function.arguments.iter() {
            arguments_names.push(arg.identifier.span.literal.clone());
        }
        self.functions.insert(
            function.identifier.span.literal.clone(),
            arguments_names.clone(),
        );

        // arguments_names.push(function.identifier.span.literal.clone());
        self.enter_scope(arguments_names);

        self.visit_statement(&function.body);
        // match &function.body.kind {
        //     super::ASTStatementKind::Compound(statement) => {
        //         for statement in statement.statements.iter() {
        //             self.visit_statement(statement);
        //         }
        //     }
        //     _ => todo!(),
        // };
        self.leave_scope();
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast,
        expr: &super::ASTAssignmentExpression,
    ) {
    }

    fn visit_function_call_expression(
        &mut self,
        ast: &mut Ast,
        expr: &super::ASTFunctionCallExpression,
    ) {
        if !self.check_identifier_in_scope(&expr.identifier().to_string()) {
            self.diagnostics
                .borrow_mut()
                .report_undefined_variable(expr.identifier.span.clone());
        }

        let expected_number_of_arguments = self.functions.get(expr.identifier()).unwrap().len();
        if expected_number_of_arguments != expr.arguments.len() {
            self.diagnostics
                .borrow_mut()
                .report_number_of_function_arguments_mismatch(
                    expr.identifier.span.clone(),
                    expected_number_of_arguments,
                    expr.arguments.len(),
                );
            return;
        }

        for arg in expr.arguments.iter() {
            self.visit_expression(arg);
        }
    }

    fn visit_variable_expression(&mut self, ast: &mut Ast, expr: &super::ASTVariableExpression) {
        if !self.check_identifier_in_scope(&expr.identifier().to_string()) {
            self.diagnostics
                .borrow_mut()
                .report_undefined_variable(expr.identifier.span.clone());
        }
    }

    fn visit_unary_expression(&mut self, ast: &mut Ast, expr: &super::ASTUnaryExpression) {
        self.visit_expression(&expr.expr);
    }

    fn visit_binary_expression(&mut self, ast: &mut Ast, expr: &super::ASTBinaryExpression) {
        self.visit_expression(&expr.left);
        self.visit_expression(&expr.right);
    }

    fn visit_parenthesised_expression(
        &mut self,
        ast: &mut Ast,
        expr: &super::ASTParenthesizedExpression,
    ) {
        self.visit_expression(&expr.expr);
    }

    fn visit_binary_operator(&mut self, op: &super::ASTBinaryOperator) {}
    fn visit_error(&mut self, span: &super::lexer::TextSpan) -> () {}
    fn visit_integer(&mut self, integer: &i64) {}
    fn visit_boolean(&mut self, boolean: bool) {}
    fn visit_float(&mut self, float: &f64) {}
}
