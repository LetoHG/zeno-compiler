use crate::ast::symbol_table;
use crate::{ast, diagnostics};
use ast::lexer::Token;
use ast::printer::ASTHiglightPrinter;
use ast::solver::ASTSolver;
use ast::symbol_checker;
use diagnostics::printer::DiagnosticsPrinter;
use diagnostics::{DiagnosticsColletion, DiagnosticsColletionCell};
use std::{cell::RefCell, rc::Rc};

use crate::source_text::SourceText;

pub struct CompilationUnit {
    pub(crate) ast: ast::Ast,
    diagnostics_colletion: DiagnosticsColletionCell,
}

impl CompilationUnit {
    pub fn compile(input: &str) -> Result<CompilationUnit, ()> {
        let source_text = SourceText::new(input.to_string());
        let mut lexer = ast::lexer::Lexer::new(input.to_string());
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }

        let diagnostics_colletion = Rc::new(RefCell::new(DiagnosticsColletion::new()));
        let mut ast = ast::Ast::new();
        let mut parser = ast::parser::Parser::new(tokens, Rc::clone(&diagnostics_colletion));
        while let Some(statement) = parser.next_statement() {
            ast.add_statement(statement);
        }
        ast.visualize();

        let mut highlight_printer = ASTHiglightPrinter::new();
        ast.visit(&mut highlight_printer);
        highlight_printer.print_result();
        {
            let count_errors = diagnostics_colletion.borrow_mut().count_errors;
            let count_warnings = diagnostics_colletion.borrow_mut().count_warnings;
            println!(
                "Syntax: {} Errors and {} Warnings",
                count_errors, count_warnings
            );
        }
        Self::check_diagstics(&source_text, &diagnostics_colletion)?;

        let mut symbol_table = symbol_table::SymbolTable::new(Rc::clone(&diagnostics_colletion));
        symbol_table.build(&ast);
        // let mut symbol_checker =
        //     symbol_checker::SymbolChecker::new(Rc::clone(&diagnostics_colletion));
        // ast.visit(&mut symbol_checker);
        {
            let count_errors = diagnostics_colletion.borrow_mut().count_errors;
            let count_warnings = diagnostics_colletion.borrow_mut().count_warnings;
            println!(
                "Type Check: {} Errors and {} Warnings",
                count_errors, count_warnings
            );
        }
        Self::check_diagstics(&source_text, &diagnostics_colletion)?;

        Ok(Self {
            ast,
            diagnostics_colletion,
        })
    }

    pub fn run(&self) {
        let mut solver = ASTSolver::new();
        self.ast.visit(&mut solver);
        // solver.print_result();
        solver.solve();
    }

    fn check_diagstics(
        source_text: &SourceText,
        diagnostics_colletion: &DiagnosticsColletionCell,
    ) -> Result<(), ()> {
        let diagnostics_messages = &diagnostics_colletion.borrow().diagnostics;
        if diagnostics_messages.len() > 0 {
            let diagnostics_printer = DiagnosticsPrinter::new(&source_text, &diagnostics_messages);
            diagnostics_printer.print();
            return Err(());
        }
        Ok(())
    }
}
