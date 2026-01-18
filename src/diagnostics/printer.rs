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

    // let b = 7 - elepant + aligator;
    //             ^^^^^^^ Not found in this scope
    pub fn stringify_diagnostic(&self, diagnostic: &Diagnostic) -> String {
        let (line, col) = self.source_text.get_location(diagnostic.span.start);
        let line_number = self.source_text.get_linenumber(diagnostic.span.start);
        let symbol_len = diagnostic.span.literal.len();
        let symbol_end_col = col + diagnostic.span.literal.len();
        let prefix = line[..col].to_string();
        let error_symbol = line[col..symbol_end_col].to_string();
        let suffix = line[symbol_end_col..].to_string();

        let message_color: Box<dyn color::Color> = match diagnostic.kind {
            super::DiagnosticKind::Error => Box::new(color::Red),
            super::DiagnosticKind::Warning => Box::new(color::Yellow),
        };

        let line_number_str = format!("{:2} | ", line_number);
        let whitespace = " ".repeat(col + line_number_str.len());

        format!(
            // "{}{line_number_str}{}{prefix}{error_symbol}{suffix}\n{whitespace}{}{}\n{whitespace}|\n{whitespace}+-- {}{}",
            "{}{line_number_str}{}{prefix}{error_symbol}{suffix}\n{whitespace}{}{} {} ({}:{}){}",
            color::Fg(color::Blue),
            color::Fg(color::Reset),
            color::Fg(message_color.as_ref()),
            "^".repeat(symbol_len),
            diagnostic.message,
            line_number,
            col,
            color::Fg(color::Reset)
        )
        .to_string()
    }
}
