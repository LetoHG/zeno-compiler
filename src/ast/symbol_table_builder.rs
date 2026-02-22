use crate::{
    ast::{
        symbol_table::{
            DataType, FunctionInfo, StructDataMember, Symbol, SymbolTable, VariableInfo,
        },
        Ast,
    },
    diagnostics::{self, DiagnosticsCollectionCell},
};

use super::ASTVisitor;

pub struct SymbolTableBuilder<'a> {
    symbol_table: &'a mut SymbolTable,
    diagnostics: DiagnosticsCollectionCell,
}

impl<'a> SymbolTableBuilder<'a> {
    pub fn new(diagnostics: DiagnosticsCollectionCell, symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            diagnostics,
        }
    }

    pub fn build(&mut self, ast: &mut Ast) {
        for stmnt_id in ast.top_level_statements.clone().iter() {
            self.visit_statement(ast, *stmnt_id);
        }
    }

    fn declare_global_identifier(&mut self, symbol: Symbol) -> bool {
        self.symbol_table.declare_global_identifier(symbol)
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbol_table.lookup(name)
    }
}

impl<'a> ASTVisitor<()> for SymbolTableBuilder<'a> {
    fn visit_return_statement(&mut self, ast: &mut Ast, statement: &super::ASTReturnStatement) {
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

    fn visit_let_statement(&mut self, ast: &mut Ast, statement: &super::ASTLetStatement) {
        let success = self.declare_global_identifier(Symbol::Constant(VariableInfo {
            name: statement.identifier.name(),
            data_type: DataType::from_token(&statement.data_type),
        }));
        if !success {
            self.diagnostics.borrow_mut().report_error(
                format!("Redefinition of global identifier"),
                statement.identifier.span.clone(),
            )
        }
    }

    fn visit_var_statement(&mut self, ast: &mut Ast, statement: &super::ASTVarStatement) {
        let success = self.declare_global_identifier(Symbol::Variable(VariableInfo {
            name: statement.identifier.name(),
            data_type: DataType::from_token(&statement.data_type),
        }));
        if !success {
            self.diagnostics.borrow_mut().report_error(
                format!("Redefinition of global identifier"),
                statement.identifier.span.clone(),
            )
        }
    }

    fn visit_compound_statement(&mut self, ast: &mut Ast, statement: &super::ASTCompoundStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("standalone Compound statement not allowed outside of functions"),
            super::lexer::TextSpan {
                start: statement.start_brace.span.start,
                end: 0,
                literal: "{".to_string(),
            },
        );
    }

    fn visit_if_statement(&mut self, ast: &mut Ast, statement: &super::ASTIfStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("If statement not allowed outside of functions"),
            statement.keyword.span.clone(),
        );
    }

    fn visit_for_loop_statement(&mut self, ast: &mut Ast, statement: &super::ASTForStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("For loop statement not allowed outside of functions"),
            statement.keyword.span.clone(),
        );
    }

    fn visit_while_loop_statement(&mut self, ast: &mut Ast, statement: &super::ASTWhileStatement) {
        self.diagnostics.borrow_mut().report_error(
            format!("While statement not allowed outside of functions"),
            statement.keyword.span.clone(),
        );
    }

    fn visit_function_statement(&mut self, ast: &mut Ast, function: &super::ASTFunctionStatement) {
        let mut argument_types: Vec<DataType> = Vec::new();
        // add arguments to scope of local variable call
        for arg in function.arguments.iter() {
            // argument_types.push(arg.identifier.span.literal.clone());
            argument_types.push(DataType::from_token(&arg.data_type));
        }
        let success = self.declare_global_identifier(Symbol::Function(FunctionInfo {
            name: function.identifier.name(),
            parameters: argument_types,
            return_type: DataType::from_token(&function.return_type),
        }));
        if !success {
            self.diagnostics.borrow_mut().report_error(
                format!("Redefinition of global identifier"),
                function.identifier.span.clone(),
            )
        }
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast,
        _assign_expr: &super::ASTAssignmentExpression,
        _expr: &super::ASTExpression,
    ) {
    }
    fn visit_function_call_expression(
        &mut self,
        ast: &mut Ast,
        _func_call_expr: &super::ASTFunctionCallExpression,
        _expr: &super::ASTExpression,
    ) {
    }
    fn visit_variable_expression(
        &mut self,
        ast: &mut Ast,
        _var_expr: &super::ASTVariableExpression,
        _expr: &super::ASTExpression,
    ) {
    }
    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast,
        _unary_expr: &super::ASTUnaryExpression,
        _expr: &super::ASTExpression,
    ) {
    }
    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast,
        _bin_expr: &super::ASTBinaryExpression,
        _expr: &super::ASTExpression,
    ) {
    }
    fn visit_parenthesised_expression(
        &mut self,
        ast: &mut Ast,
        _paren_expr: &super::ASTParenthesizedExpression,
        _expr: &super::ASTExpression,
    ) {
    }
    fn visit_binary_operator(&mut self, _op: &super::ASTBinaryOperator) {}
    fn visit_error(&mut self, _span: &super::lexer::TextSpan) {}
    fn visit_integer(&mut self, _integer: &i64, _expr: &super::ASTExpression) {}
    fn visit_boolean(&mut self, _boolean: bool, _expr: &super::ASTExpression) {}
    fn visit_float(&mut self, _float: &f64, _expr: &super::ASTExpression) {}
}
