use core::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // Litarals
    Integer(i64),
    Floating(f64),
    Identifier,

    // Keywords
    Let,
    Var,
    Func,
    Return,
    If,
    Else,
    For,
    In,
    While,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    Str,
    Struct,
    Void,
    Null,

    // Arithmetic Operators
    Plus,
    Minus,
    Astrisk,
    Slash,
    Equal,

    PlusEqual,
    MinusEqual,
    AstriskEqual,
    SlashEqual,

    // Bitwise Operators
    Pipe,           // OR
    Ampersand,      // AND
    Caret,          // XOR
    Tilde,          // NOT
    PipeEqual,      // OR self and assign
    AmpersandEqual, // AND self and assign
    CaretEqual,     // XOR self and assign

    // Logical Operators
    ExclemationMark,        // not
    ExclemationMarkEqual,   // not equal to
    EqualEqual,             // equal to
    AmpersandAmpersand,     // AND
    PipePipe,               // OR
    LeftAngleBracket,       // greater than
    LeftAngleBracketEqual,  // greater than or equal
    RightAngleBracket,      // less than
    RightAngleBracketEqual, // less than or equal

    // Misc & other lexical symbols
    SingleLineComment(String),
    MultiLineComment(String),
    SlashSlash,
    SlashAstrisk,
    AstriskSlash,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    MinusRightAngleBracket, // -> for return types
    Comma,
    Dot,
    SemiColon,
    Colon,

    Whitespace,
    Bad,
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Identifier => write!(f, "Identifier"),
            TokenKind::Let => write!(f, "Let"),
            TokenKind::Var => write!(f, "Var"),
            TokenKind::Func => write!(f, "Func"),
            TokenKind::Return => write!(f, "Return"),
            TokenKind::If => write!(f, "If"),
            TokenKind::Else => write!(f, "Else"),
            TokenKind::For => write!(f, "For"),
            TokenKind::In => write!(f, "In"),
            TokenKind::While => write!(f, "While"),
            TokenKind::I8 => write!(f, "I8"),
            TokenKind::I16 => write!(f, "I16"),
            TokenKind::I32 => write!(f, "I32"),
            TokenKind::I64 => write!(f, "I64"),
            TokenKind::U8 => write!(f, "U8"),
            TokenKind::U16 => write!(f, "U16"),
            TokenKind::U32 => write!(f, "U32"),
            TokenKind::U64 => write!(f, "U64"),
            TokenKind::F32 => write!(f, "F32"),
            TokenKind::F64 => write!(f, "F64"),
            TokenKind::Bool => write!(f, "Bool"),
            TokenKind::Char => write!(f, "Char"),
            TokenKind::Str => write!(f, "Str"),
            TokenKind::Struct => write!(f, "Struct"),
            TokenKind::Void => write!(f, "Void"),
            TokenKind::Null => write!(f, "Null"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Astrisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::PlusEqual => write!(f, "+="),
            TokenKind::MinusEqual => write!(f, "-="),
            TokenKind::AstriskEqual => write!(f, "*="),
            TokenKind::SlashEqual => write!(f, "/="),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::PipeEqual => write!(f, "|="),
            TokenKind::AmpersandEqual => write!(f, "&="),
            TokenKind::CaretEqual => write!(f, "^="),
            TokenKind::ExclemationMark => write!(f, "!"),
            TokenKind::ExclemationMarkEqual => write!(f, "!="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::AmpersandAmpersand => write!(f, "&&"),
            TokenKind::PipePipe => write!(f, "||"),
            TokenKind::LeftAngleBracket => write!(f, ">"),
            TokenKind::LeftAngleBracketEqual => write!(f, "<="),
            TokenKind::RightAngleBracket => write!(f, ">"),
            TokenKind::RightAngleBracketEqual => write!(f, ">="),
            TokenKind::SlashSlash => write!(f, "//"),
            TokenKind::SlashAstrisk => write!(f, "/*"),
            TokenKind::AstriskSlash => write!(f, "*/"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{}", '{'),
            TokenKind::RightBrace => write!(f, "{}", '}'),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::MinusRightAngleBracket => write!(f, "->"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Whitespace => write!(f, "Whitespace"),
            TokenKind::Bad => write!(f, "Bad"),
            TokenKind::Eof => write!(f, "Eof"),
            TokenKind::SingleLineComment(_) => write!(f, "//"),
            TokenKind::MultiLineComment(_) => write!(f, "/* */"),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) literal: String,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub(crate) span: TextSpan,
}

impl Token {
    fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self { kind, span }
    }
    pub(crate) fn name(&self) -> String {
        self.span.literal.clone()
    }
}

pub struct Lexer {
    input: String,
    cursor: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self { input, cursor: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.cursor > self.input.len() {
            return None;
        }
        if self.cursor == self.input.len() {
            self.cursor += 1;
            return Some(Token::new(
                TokenKind::Eof,
                TextSpan::new(0, 0, '\0'.to_string()),
            ));
        }

        let start = self.cursor;
        let mut kind = TokenKind::Bad;
        let c = self.current_char()?;

        if Self::is_number_start(&c) {
            kind = self.consume_number();
        } else if Self::is_identifier_start(&c) {
            let identifier = self.consume_identifier();
            kind = match identifier.as_str() {
                "let" => TokenKind::Let,
                "var" => TokenKind::Var,
                "func" => TokenKind::Func,
                "return" => TokenKind::Return,
                "if" => TokenKind::If,
                "else" => TokenKind::Else,
                "for" => TokenKind::For,
                "in" => TokenKind::In,
                "while" => TokenKind::While,
                "i8" => TokenKind::I8,
                "i16" => TokenKind::I16,
                "i32" => TokenKind::I32,
                "i64" => TokenKind::I64,
                "u8" => TokenKind::U8,
                "u16" => TokenKind::U16,
                "u32" => TokenKind::U32,
                "u64" => TokenKind::U64,
                "f32" => TokenKind::F32,
                "f64" => TokenKind::F64,
                "bool" => TokenKind::Bool,
                "char" => TokenKind::Char,
                "str" => TokenKind::Str,
                "struct" => TokenKind::Struct,
                "void" => TokenKind::Void,
                "null" => TokenKind::Null,
                _ => TokenKind::Identifier,
            };
        } else if c == '/' && self.peek(1)? == '/' {
            kind = self.consume_single_line_comment();
        } else if c == '/' && self.peek(1)? == '*' {
            kind = self.consume_multi_line_comment();
        } else if Self::is_whitespace(&c) {
            self.consume();
            kind = TokenKind::Whitespace;
        } else {
            kind = self.consume_punctuation();
        }

        let end = self.cursor;
        let literal = self.input[start..end].to_string();

        Some(Token::new(kind, TextSpan::new(start, end, literal)))
    }

    fn is_number_start(c: &char) -> bool {
        c.is_digit(10)
    }

    fn is_identifier_start(c: &char) -> bool {
        c.is_alphabetic() || *c == '_'
    }

    fn is_whitespace(c: &char) -> bool {
        c.is_whitespace()
    }

    fn is_decimal_dot(c: &char) -> bool {
        *c == '.'
    }

    fn is_linebreak(c: &char) -> bool {
        *c == '\n'
    }

    fn current_char(&mut self) -> Option<char> {
        self.peek(0)
    }

    fn consume(&mut self) -> Option<char> {
        if self.cursor >= self.input.len() {
            return None;
        }
        let c = self.current_char();
        self.cursor += 1;
        c
    }

    fn peek(&mut self, offset: usize) -> Option<char> {
        if self.cursor + offset >= self.input.len() {
            return None;
        }
        self.input.chars().nth(self.cursor + offset)
    }

    fn consume_single_line_comment(&mut self) -> TokenKind {
        self.consume();
        self.consume();
        let mut comment: String = "".to_string();
        while let Some(c) = self.consume() {
            comment.push(c);
            if Self::is_linebreak(&c) {
                break;
            }
        }
        TokenKind::SingleLineComment(comment)
    }

    fn consume_multi_line_comment(&mut self) -> TokenKind {
        self.consume();
        self.consume();
        let mut comment: String = "".to_string();
        while let Some(c) = self.consume() {
            if c == '*' {
                if let Some(slash) = self.current_char() {
                    if slash == '/' {
                        self.consume();
                        break;
                    }
                }
            }
            comment.push(c);
        }
        TokenKind::MultiLineComment(comment)
    }

    fn consume_number(&mut self) -> TokenKind {
        let mut integer_part: i64 = 0;
        let mut fractional_part: i64 = 0;
        let mut divisior_for_fraction: i64 = 1;

        let mut oct_format = false;
        let mut hex_format = false;
        if self.current_char().unwrap() == '0' {
            if self.peek(1).unwrap().is_digit(8) {
                self.consume();
                oct_format = true;
            }

            if self.current_char().unwrap() == 'x' {
                self.consume();
                self.consume();
                hex_format = true;
                oct_format = false;
            }
        }

        let mut dot_found = false;

        while let Some(c) = self.current_char() {
            if !dot_found && oct_format && c.is_digit(8) {
                self.consume();
                integer_part = integer_part * 8 + c.to_digit(8).unwrap() as i64;
            } else if !dot_found && hex_format && c.is_digit(16) {
                self.consume();
                integer_part = integer_part * 16 + c.to_digit(16).unwrap() as i64;
            } else if c.is_digit(10) {
                self.consume();
                if !dot_found {
                    integer_part = integer_part * 10 + c.to_digit(10).unwrap() as i64;
                } else {
                    fractional_part = fractional_part * 10 + c.to_digit(10).unwrap() as i64;
                    divisior_for_fraction *= 10;
                }
            } else if !oct_format
                && !hex_format
                && Self::is_decimal_dot(&c)
                && self.peek(1).unwrap().is_digit(10)
            {
                self.consume();
                if dot_found {
                    break;
                }
                dot_found = true;
            } else {
                break;
            }
        }
        if dot_found {
            return TokenKind::Floating(
                integer_part as f64 + (fractional_part as f64 / divisior_for_fraction as f64),
            );
        } else {
            return TokenKind::Integer(integer_part);
        }
    }

    fn consume_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(c) = self.current_char() {
            if c.is_alphanumeric() || c == '_' {
                identifier.push(c);
                self.consume();
            } else {
                break;
            }
        }
        identifier
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        match self.consume().unwrap() {
            '+' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::PlusEqual;
                }
                TokenKind::Plus
            }
            '-' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::MinusEqual;
                }
                if self.current_char().unwrap() == '>' {
                    self.consume();
                    return TokenKind::MinusRightAngleBracket;
                }
                TokenKind::Minus
            }
            '*' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::AstriskEqual;
                }
                TokenKind::Astrisk
            }
            '/' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::SlashEqual;
                }
                if self.current_char().unwrap() == '/' {
                    self.consume();
                    return TokenKind::SlashSlash;
                }
                if self.current_char().unwrap() == '*' {
                    self.consume();
                    return TokenKind::SlashAstrisk;
                }
                TokenKind::Slash
            }
            '=' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::EqualEqual;
                }
                TokenKind::Equal
            }
            '|' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::PipeEqual;
                }
                if self.current_char().unwrap() == '|' {
                    self.consume();
                    return TokenKind::PipePipe;
                }
                TokenKind::Pipe
            }
            '&' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::AstriskEqual;
                }
                if self.current_char().unwrap() == '&' {
                    self.consume();
                    return TokenKind::AmpersandAmpersand;
                }
                TokenKind::Ampersand
            }
            '^' => {
                if self.current_char().unwrap() == '^' {
                    self.consume();
                    return TokenKind::CaretEqual;
                }
                TokenKind::Caret
            }
            '~' => TokenKind::Tilde,
            '!' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::ExclemationMarkEqual;
                }
                TokenKind::ExclemationMark
            }
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '<' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::LeftAngleBracketEqual;
                }
                TokenKind::LeftAngleBracket
            }
            '>' => {
                if self.current_char().unwrap() == '=' {
                    self.consume();
                    return TokenKind::RightAngleBracketEqual;
                }
                TokenKind::RightAngleBracket
            }
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            ';' => TokenKind::SemiColon,
            ':' => TokenKind::Colon,
            _ => TokenKind::Bad,
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, TextSpan, Token, TokenKind};

    fn verify(input: &str, expected_tokens: Vec<Token>) {
        let mut lexer = Lexer::new(input.to_string());
        let mut lexed_tokens = Vec::new();
        while let Some(token) = lexer.next_token() {
            if token.kind == TokenKind::Whitespace {
                continue;
            }
            println!("{:?}", token);
            lexed_tokens.push(token);
        }

        assert_eq!(
            expected_tokens.len(),
            lexed_tokens.len(),
            "Expected {} tokens but found {} ",
            expected_tokens.len(),
            lexed_tokens.len()
        );

        for (ac, ex) in lexed_tokens.iter().zip(expected_tokens.iter()) {
            println!("Ac: {:?}  Ex: {:?}", ac, ex);
            assert_eq!(
                ac.kind, ex.kind,
                "Tokens do not match. Expected {:?} but found {:?}",
                ex.kind, ac.kind
            );

            if ex.span.literal.len() > 0 {
                assert_eq!(
                    ac.span.literal, *ex.span.literal,
                    "Tokens do not match. Expected {:?} but found {:?}",
                    ex.span.literal, ac.span.literal
                );
            }
        }
    }

    fn token(kind: TokenKind, literal: &str) -> Token {
        Token {
            kind,
            span: TextSpan {
                start: 0,
                end: 0,
                literal: literal.to_string(),
            },
        }
    }

    #[test]
    fn lex_basic_let_statements() {
        let input = "let a: u8 = 10;";
        let expected_tokens = vec![
            token(TokenKind::Let, ""),
            token(TokenKind::Identifier, "a"),
            token(TokenKind::Colon, ""),
            token(TokenKind::U8, ""),
            token(TokenKind::Equal, ""),
            token(TokenKind::Integer(10), ""),
            token(TokenKind::SemiColon, ""),
            token(TokenKind::Eof, ""),
        ];

        verify(input, expected_tokens);
    }

    #[test]
    fn lex_basic_var_statements() {
        let input = "var _abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890: f32 = 10.156464 + 7 * (27 / 5.1);";
        let expected_tokens = vec![
            token(TokenKind::Var, ""),
            token(
                TokenKind::Identifier,
                "_abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890",
            ),
            token(TokenKind::Colon, ""),
            token(TokenKind::F32, ""),
            token(TokenKind::Equal, ""),
            token(TokenKind::Floating(10.156464), ""),
            token(TokenKind::Plus, ""),
            token(TokenKind::Integer(7), "7"),
            token(TokenKind::Astrisk, ""),
            token(TokenKind::LeftParen, ""),
            token(TokenKind::Integer(27), "27"),
            token(TokenKind::Slash, ""),
            token(TokenKind::Floating(5.1), "5.1"),
            token(TokenKind::RightParen, ""),
            token(TokenKind::SemiColon, ""),
            token(TokenKind::Eof, ""),
        ];

        verify(input, expected_tokens);
    }

    #[test]
    fn lex_basic_func_decl_statements() {
        let input = "func _my_func(a: str, b: u8) -> str {
            return a;
        }
        ";
        let expected_tokens = vec![
            token(TokenKind::Func, ""),
            token(TokenKind::Identifier, "_my_func"),
            token(TokenKind::LeftParen, ""),
            token(TokenKind::Identifier, "a"),
            token(TokenKind::Colon, ""),
            token(TokenKind::Str, ""),
            token(TokenKind::Comma, ""),
            token(TokenKind::Identifier, "b"),
            token(TokenKind::Colon, ""),
            token(TokenKind::U8, ""),
            token(TokenKind::RightParen, ""),
            token(TokenKind::MinusRightAngleBracket, ""),
            token(TokenKind::Str, ""),
            token(TokenKind::LeftBrace, ""),
            token(TokenKind::Return, ""),
            token(TokenKind::Identifier, "a"),
            token(TokenKind::SemiColon, ""),
            token(TokenKind::RightBrace, ""),
            token(TokenKind::Eof, ""),
        ];

        verify(input, expected_tokens);
    }
}
