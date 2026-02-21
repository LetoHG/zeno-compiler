pub mod printer;

use crate::ast::lexer::{TextSpan, Token, TokenKind};
use std::{cell::RefCell, rc::Rc};

pub enum DiagnosticKind {
    Error,
    Warning,
}

impl DiagnosticKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            DiagnosticKind::Error => "Error",
            DiagnosticKind::Warning => "Warning",
        }
    }
}

pub enum DiagnosticMessage {
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },
    ExpectedExpression {
        found: TokenKind,
    },
    UndefinedFunction {
        name: String,
    },
    UndefinedVariable {
        name: String,
    },
    UndefinedIdentifier {
        name: String,
    },
    NotACallable {
        name: String,
    },
    TypeMismatch {
        expected: String,
        found: String,
    },
    NumberOfFunctionArgumentsMismatch {
        expected: usize,
        found: usize,
    },
    Custom(String),
}

pub struct Diagnostic {
    pub(crate) message: DiagnosticMessage,
    pub(crate) kind: DiagnosticKind,
    pub(crate) span: TextSpan,
}

impl Diagnostic {
    pub fn new(message: DiagnosticMessage, kind: DiagnosticKind, span: TextSpan) -> Self {
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

    pub fn report_error(&mut self, message: DiagnosticMessage, span: TextSpan) {
        self.count_errors += 1;
        self.diagnostics
            .push(Diagnostic::new(message, DiagnosticKind::Error, span));
    }

    pub fn report_custom_warning(&mut self, message: String, span: TextSpan) {
        self.count_warnings += 1;
        self.diagnostics.push(Diagnostic::new(
            DiagnosticMessage::Custom(message),
            DiagnosticKind::Warning,
            span,
        ));
    }

    pub fn report_custom_error(&mut self, message: String, span: TextSpan) {
        self.count_errors += 1;
        self.diagnostics.push(Diagnostic::new(
            DiagnosticMessage::Custom(message),
            DiagnosticKind::Error,
            span,
        ));
    }

    pub fn report_unexpected_token(&mut self, expected_tokenkind: &TokenKind, found_token: &Token) {
        self.report_error(
            DiagnosticMessage::UnexpectedToken {
                expected: expected_tokenkind.clone(),
                found: found_token.kind.clone(),
            },
            found_token.span.clone(),
        );
    }
    pub fn report_expected_expression(&mut self, found_token: &Token) {
        self.report_error(
            DiagnosticMessage::ExpectedExpression {
                found: found_token.kind.clone(),
            },
            found_token.span.clone(),
        );
    }

    pub fn report_undefined_function(&mut self, span: TextSpan) {
        self.report_error(
            DiagnosticMessage::UndefinedFunction {
                name: span.literal.clone(),
            },
            span,
        );
    }

    pub fn report_undefined_variable(&mut self, span: TextSpan) {
        self.report_error(
            DiagnosticMessage::UndefinedVariable {
                name: span.literal.clone(),
            },
            span,
        );
    }

    pub fn report_undefined_identifier(&mut self, span: TextSpan) {
        self.report_error(
            DiagnosticMessage::UndefinedIdentifier {
                name: span.literal.clone(),
            },
            span,
        );
    }

    pub fn report_not_a_callable(&mut self, span: TextSpan) {
        self.report_error(
            DiagnosticMessage::NotACallable {
                name: span.literal.clone(),
            },
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
            DiagnosticMessage::TypeMismatch {
                expected: expected_type,
                found: found_type,
            },
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
            DiagnosticMessage::NumberOfFunctionArgumentsMismatch { expected, found },
            span,
        );
    }
}
