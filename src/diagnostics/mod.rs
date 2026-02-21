pub mod printer;

use crate::ast::lexer::{TextSpan, Token, TokenKind};
use std::{cell::RefCell, rc::Rc};

pub enum DiagnosticKind {
    Error,
    Warning,
}

pub struct Diagnostic {
    pub(crate) message: String,
    pub(crate) kind: DiagnosticKind,
    pub(crate) span: TextSpan,
}

impl Diagnostic {
    pub fn new(message: String, kind: DiagnosticKind, span: TextSpan) -> Self {
        Self {
            message,
            kind,
            span,
        }
    }
}

pub struct DiagnosticsCollection {
    pub diagnostics: Vec<Diagnostic>,
    pub count_errors: usize,
    pub count_warnings: usize,
}

pub type DiagnosticsCollectionCell = Rc<RefCell<DiagnosticsCollection>>;

impl DiagnosticsCollection {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
            count_errors: 0,
            count_warnings: 0,
        }
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.count_errors = 0;
        self.count_warnings = 0;
    }

    pub fn report_error(&mut self, message: String, span: TextSpan) {
        self.count_errors += 1;
        self.diagnostics
            .push(Diagnostic::new(message, DiagnosticKind::Error, span));
    }

    pub fn report_warning(&mut self, message: String, span: TextSpan) {
        self.count_warnings += 1;
        self.diagnostics
            .push(Diagnostic::new(message, DiagnosticKind::Warning, span));
    }

    pub fn report_unexpected_token(&mut self, expected_tokenkind: &TokenKind, found_token: &Token) {
        self.report_error(
            format!(
                "Expected <{}>, but found <{}>",
                expected_tokenkind, found_token.kind
            ),
            found_token.span.clone(),
        );
    }
    pub fn report_expected_expression(&mut self, found_token: &Token) {
        self.report_error(
            format!("Expected expression, but found <{}>", found_token.kind),
            found_token.span.clone(),
        );
    }

    pub fn report_undefined_function(&mut self, span: TextSpan) {
        self.report_error(format!("Undefined function: {}", span.literal), span);
    }

    pub fn report_undefined_variable(&mut self, span: TextSpan) {
        self.report_error(format!("Not found in this scope"), span);
    }

    pub fn report_undefined_identifier(&mut self, span: TextSpan) {
        self.report_error(
            format!("No identifier named '{}' in scope", span.literal),
            span,
        );
    }

    pub fn report_not_a_callable(&mut self, span: TextSpan) {
        self.report_error(
            format!("Identifier '{}' is not callable", span.literal),
            span,
        );
    }
    pub fn report_type_mismatch(
        &mut self,
        span: TextSpan,
        found_type: String,
        expected_type: String,
    ) {
        self.report_error(
            format!(
                "Type mismatch. Expected <{}>, but found <{}>",
                expected_type, found_type
            ),
            span,
        );
    }

    pub fn report_number_of_function_arguments_mismatch(
        &mut self,
        span: TextSpan,
        expected: usize,
        found: usize,
    ) {
        self.report_error(
            format!(
                "Function {} expects {} arguments but {} were given",
                span.literal, expected, found
            ),
            span,
        );
    }
}
