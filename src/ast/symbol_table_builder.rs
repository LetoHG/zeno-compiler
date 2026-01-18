use crate::{
    ast::symbol_table::{DataType, FunctionInfo, Symbol, SymbolTable, VariableInfo},
    diagnostics::DiagnosticsColletionCell,
};

use super::ASTVisitor;

pub struct SymbolTableBuilder<'a> {
    symbol_table: &'a mut SymbolTable,
    diagnostics: DiagnosticsColletionCell,
}

impl<'a> SymbolTableBuilder<'a> {
    pub fn new(diagnostics: DiagnosticsColletionCell, symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            diagnostics,
        }
    }
    pub fn build(&mut self, ast: &super::Ast) {
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

impl<'a> ASTVisitor<()> for SymbolTableBuilder<'a> {
    fn visit_return_statement(&mut self, statement: &super::ASTReturnStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("Return statement not allowed outside of functions"),
            // TODO(letohg): [2026-01-16] diagnostic print does not work without the +1
            super::lexer::TextSpan {
                start: statement.keyword.span.start + 1,
                end: statement.keyword.span.end,
                literal: "return".to_string(),
            },
        );
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) {
        self.declare_global_identifier(Symbol::Constant(VariableInfo {
            name: statement.identifier.name(),
            data_type: DataType::from_token(&statement.data_type),
        }));
    }

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) {
        self.declare_global_identifier(Symbol::Variable(VariableInfo {
            name: statement.identifier.name(),
            data_type: DataType::from_token(&statement.data_type),
        }));
    }

    fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("standalone Compound statement not allowed outside of functions"),
            super::lexer::TextSpan {
                start: statement.start_brace.span.start,
                end: 0,
                literal: "{".to_string(),
            },
        );
    }

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("If statement not allowed outside of functions"),
            statement.keyword.span.clone(),
        );
    }

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("For loop statement not allowed outside of functions"),
            statement.keyword.span.clone(),
        );
    }

    fn visit_while_loop_statement(&mut self, statement: &super::ASTWhileStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("While statement not allowed outside of functions"),
            statement.keyword.span.clone(),
        );
    }

    fn visit_function_statement(&mut self, function: &super::ASTFunctionStatement) {
        let mut argument_types: Vec<DataType> = Vec::new();
        // add arguments to scope of local variable call
        for arg in function.arguments.iter() {
            // argument_types.push(arg.identifier.span.literal.clone());
            argument_types.push(DataType::from_token(&arg.data_type));
        }
        self.declare_global_identifier(Symbol::Function(FunctionInfo {
            name: function.identifier.name(),
            parameters: argument_types,
            return_type: DataType::from_token(&function.return_type),
        }));
    }

    fn visit_assignment_expression(&mut self, _expr: &super::ASTAssignmentExpression) {}
    fn visit_function_call_expression(&mut self, _expr: &super::ASTFunctionCallExpression) {}
    fn visit_variable_expression(&mut self, _expr: &super::ASTVariableExpression) {}
    fn visit_unary_expression(&mut self, _expr: &super::ASTUnaryExpression) {}
    fn visit_binary_expression(&mut self, _expr: &super::ASTBinaryExpression) {}
    fn visit_parenthesised_expression(&mut self, _expr: &super::ASTParenthesizedExpression) {}
    fn visit_binary_operator(&mut self, _op: &super::ASTBinaryOperator) {}
    fn visit_error(&mut self, _span: &super::lexer::TextSpan) {}
    fn visit_integer(&mut self, _integer: &i64) {}
    fn visit_boolean(&mut self, _boolean: bool) {}
    fn visit_float(&mut self, _float: &f64) {}
}
