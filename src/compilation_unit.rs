use crate::ast::symbol_table;
use crate::ast::symbol_table_builder;
use crate::ast::type_checker;
use crate::{ast, diagnostics};
use ast::lexer::Token;
use ast::printer::ASTHiglightPrinter;
use ast::solver::ASTSolver;
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
        let mut parser =
            ast::parser::Parser::new(tokens, Rc::clone(&diagnostics_colletion), &mut ast);
        parser.parse();
        ast.visualize();

        let mut highlight_printer = ASTHiglightPrinter::new();
        highlight_printer.do_print(&mut ast);
        Self::check_diagstics("Parser", &source_text, &diagnostics_colletion)?;

        let mut symbol_table = symbol_table::SymbolTable::new();
        let mut symbol_table_builder = symbol_table_builder::SymbolTableBuilder::new(
            Rc::clone(&diagnostics_colletion),
            &mut symbol_table,
        );
        symbol_table_builder.build(&mut ast);
        Self::check_diagstics("SymbolTableBuilder", &source_text, &diagnostics_colletion)?;

        let mut type_checker =
            type_checker::TypeChecker::new(Rc::clone(&diagnostics_colletion), &mut symbol_table);
        type_checker.analyze(&mut ast);
        Self::check_diagstics("TypeChecker", &source_text, &diagnostics_colletion)?;

        Ok(Self {
            ast,
            diagnostics_colletion,
        })
    }

    pub fn run(&self) {
        let mut solver = ASTSolver::new();
        // solver.solve(&mut self.ast);
        // self.ast.visit(&mut solver);
        // // solver.print_result();
        // solver.solve();
    }

    fn check_diagstics(
        stage_name: &str,
        source_text: &SourceText,
        diagnostics_colletion: &DiagnosticsColletionCell,
    ) -> Result<(), ()> {
        let diagnostics_messages = &diagnostics_colletion.borrow().diagnostics;
        let count_errors = &diagnostics_colletion.borrow().count_errors;
        let count_warnings = &diagnostics_colletion.borrow().count_warnings;
        println!(
            "{}: {} Errors and {} Warnings",
            stage_name, count_errors, count_warnings
        );

        if diagnostics_messages.len() > 0 {
            let diagnostics_printer = DiagnosticsPrinter::new(&source_text, &diagnostics_messages);
            diagnostics_printer.print();
            return Err(());
        }
        Ok(())
    }
}
