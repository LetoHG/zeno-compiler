use super::Diagnostic;
use crate::source_text::SourceText;
use termion::color;

pub struct DiagnosticsPrinter<'a> {
    source_text: &'a SourceText,
    diagnostics: &'a [Diagnostic],
}

impl<'a> DiagnosticsPrinter<'a> {
    pub fn new(source_text: &'a SourceText, diagnostics: &'a [Diagnostic]) -> Self {
        Self {
            source_text,
            diagnostics,
        }
    }

    pub fn print(&self) {
        for diagnostic in self.diagnostics {
            println!("{}", self.stringify_diagnostic(diagnostic));
        }
    }

    fn format_diagnostic_message(&self, diagnostic: &Diagnostic) -> String {
        match &diagnostic.message {
            super::DiagnosticMessage::UnexpectedToken { expected, found } => {
                format!("Expected token '{}', but found '{}'", expected, found)
            }
            super::DiagnosticMessage::ExpectedExpression { found } => {
                format!("Expected an expression, but found '{}'", found)
            }
            super::DiagnosticMessage::UndefinedFunction { name } => {
                format!("Undefined function '{}'", name)
            }
            super::DiagnosticMessage::UndefinedVariable { name } => {
                format!("Undefined variable '{}'", name)
            }
            super::DiagnosticMessage::UndefinedIdentifier { name } => {
                format!("Undefined identifier '{}'", name)
            }
            super::DiagnosticMessage::NotACallable { name } => {
                format!("'{}' is not callable", name)
            }
            super::DiagnosticMessage::TypeMismatch { expected, found } => format!(
                "Type mismatch: expected '{}', but found '{}'",
                expected, found
            ),
            super::DiagnosticMessage::NumberOfFunctionArgumentsMismatch { expected, found } => {
                format!(
                    "Number of function arguments mismatch: expected {}, but found {}",
                    expected, found
                )
            }
            super::DiagnosticMessage::Custom(msg) => msg.clone(),
        }
    }
    // let b = 7 - elepant + aligator;
    //             ^^^^^^^ Not found in this scope
    pub fn stringify_diagnostic(&self, diagnostic: &Diagnostic) -> String {
        let (line, col) = self.source_text.get_location(diagnostic.span.start);
        let line_number = self.source_text.get_linenumber(diagnostic.span.start);
        let symbol_len = diagnostic.span.length();
        let symbol_end_col = col + diagnostic.span.literal.len();
        let prefix = line[..col].to_string();
        let error_symbol = line[col..symbol_end_col].to_string();
        let suffix = line[symbol_end_col..].to_string();

        let message_color: Box<dyn color::Color> = match diagnostic.kind {
            super::DiagnosticKind::Error => Box::new(color::Red),
            super::DiagnosticKind::Warning => Box::new(color::Yellow),
        };

        let line_number_str = format!("{:2} | ", line_number);
        let line_prefix = "   | ";
        let whitespace = " ".repeat(col);
        let message = self.format_diagnostic_message(diagnostic);

        // Example output:
        // error[E012]: type mismatch
        //   --> main.my:12:15
        //    |
        // 12 |     let x: u8 = 300;
        //    |               ^^^ expected `u8`, found integer literal
        //    |
        //    = note: `u8` values must be between 0 and 255
        format!(
            "{}{}{}:{}\n  --> {}:{}:{}\n{}{}\n{}{}{}\n{}{}{}{}{} {}{}",
            color::Fg(message_color.as_ref()),
            diagnostic.kind.as_str(),
            color::Fg(color::Reset),
            "message placeholder",
            self.source_text.get_filename(),
            line_number,
            col,
            color::Fg(color::Blue),
            line_prefix,
            line_number_str,
            color::Fg(color::Reset),
            line,
            color::Fg(color::Blue),
            line_prefix,
            whitespace,
            color::Fg(message_color.as_ref()),
            "^".repeat(symbol_len),
            message,
            color::Fg(color::Reset),
        )
        .to_string()

        // format!(
        //     // "{}{line_number_str}{}{prefix}{error_symbol}{suffix}\n{whitespace}{}{}\n{whitespace}|\n{whitespace}+-- {}{}",
        //     "{}{line_number_str}{}{prefix}{error_symbol}{suffix}\n{whitespace}{}{} {} ({}:{}){}",
        //     color::Fg(color::Blue),
        //     color::Fg(color::Reset),
        //     color::Fg(message_color.as_ref()),
        //     "^".repeat(symbol_len),
        //     message,
        //     line_number,
        //     col,
        //     color::Fg(color::Reset)
        // )
        // .to_string()
    }
}
