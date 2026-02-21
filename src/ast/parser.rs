use crate::ast::lexer::{Lexer, Token, TokenKind};
use crate::ast::{ASTExpression, ASTStatement, StmntId};
use crate::diagnostics::DiagnosticsCollection;
use crate::diagnostics::DiagnosticsCollectionCell;
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use super::lexer::TextSpan;
use super::{
    ASTBinaryOperator, ASTBinaryOperatorKind, ASTElseStatement, ASTUnaryOperator,
    ASTUnaryOperatorKind, Ast, ExprId, FunctionArgumentDeclaration,
};

struct Cursor {
    cursor: Cell<usize>,
}

impl Cursor {
    fn new() -> Self {
        Self {
            cursor: Cell::new(0),
        }
    }

    fn move_forward(&self) {
        let value = self.cursor.get();
        self.cursor.set(value + 1);
    }

    fn get_value(&self) -> usize {
        self.cursor.get()
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    cursor: Cursor,
    diagnostics_colletion: DiagnosticsCollectionCell,
    ast: &'a mut Ast,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        diagnostics_colletion: Rc<RefCell<DiagnosticsCollection>>,
        ast: &'a mut Ast,
    ) -> Self {
        Self {
            tokens: tokens
                .iter()
                .filter(|token| match token.kind {
                    TokenKind::Whitespace => false,
                    TokenKind::SingleLineComment(_) => false,
                    TokenKind::MultiLineComment(_) => false,
                    _ => true,
                })
                .map(|token| token.clone())
                .collect(),
            cursor: Cursor::new(),
            diagnostics_colletion,
            ast,
        }
    }

    pub fn from_input(
        input: String,
        diagnostics_colletion: DiagnosticsCollectionCell,
        ast: &'a mut Ast,
    ) -> Self {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        while let Some(token) = lexer.next_token() {
            match token.kind {
                TokenKind::Whitespace => (),
                TokenKind::SingleLineComment(_) => (),
                TokenKind::MultiLineComment(_) => (),
                _ => tokens.push(token),
            };
        }
        Self {
            tokens,
            cursor: Cursor::new(),
            diagnostics_colletion,
            ast,
        }
    }

    pub fn parse(&mut self) {
        let mut top_level = Vec::new();
        while let Some(stmnt) = self.next_statement() {
            top_level.push(stmnt.id);
        }
        self.ast.top_level_statements = top_level;
    }

    fn next_statement(&mut self) -> Option<&ASTStatement> {
        if self.current_token().kind == TokenKind::Eof {
            return None;
        }
        self.parse_statement()
    }

    fn parse_statement(&mut self) -> Option<&ASTStatement> {
        match self.current_token().kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Var => self.parse_var_statement(),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Func => self.parse_function_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_loop_statement(),
            TokenKind::For => self.parse_for_loop_statement(),
            TokenKind::LeftBrace => self.parse_compound_statement(),
            TokenKind::SingleLineComment(_) => todo!("Decide if comments need to be in AST"),
            TokenKind::MultiLineComment(_) => todo!("Decide if comments need to be in AST"),
            _ => self.parse_expression_statement(),
        }
    }

    fn current_token(&self) -> &Token {
        self.peek(0)
    }

    fn peek(&self, offset: isize) -> &Token {
        let index = std::cmp::min(
            (self.cursor.get_value() as isize + offset) as usize,
            self.tokens.len() - 1,
        );
        self.tokens.get(index).unwrap()
    }

    fn consume(&self) -> &Token {
        self.cursor.move_forward();
        self.peek(-1)
    }

    fn consume_expected(&self, expected: TokenKind) -> Option<&Token> {
        let token = self.consume();
        if token.kind != expected {
            self.diagnostics_colletion
                .borrow_mut()
                .report_unexpected_token(&expected, token);
            return None;
        }
        Some(token)
    }

    fn parse_return_statement(&mut self) -> Option<&ASTStatement> {
        let keyword = self.consume_expected(TokenKind::Return)?.clone();
        let expr = self.parse_expression()?.id;
        self.consume_expected(TokenKind::SemiColon)?;
        self.ast.return_statement(keyword, expr)
    }

    fn parse_let_statement(&mut self) -> Option<&ASTStatement> {
        self.consume_expected(TokenKind::Let)?;
        let identifier = self.consume_expected(TokenKind::Identifier)?.clone();
        self.consume_expected(TokenKind::Colon)?;
        let data_type = self.consume().clone();
        self.consume_expected(TokenKind::Equal)?;
        let expr = self.parse_expression()?.id;
        self.consume_expected(TokenKind::SemiColon)?;
        self.ast.let_statement(identifier, data_type, expr)
    }

    fn parse_var_statement(&mut self) -> Option<&ASTStatement> {
        self.consume_expected(TokenKind::Var)?;
        let identifier = self.consume_expected(TokenKind::Identifier)?.clone();
        self.consume_expected(TokenKind::Colon)?;
        let data_type = self.consume().clone();
        self.consume_expected(TokenKind::Equal)?;
        let expr = self.parse_expression()?.id;
        self.consume_expected(TokenKind::SemiColon)?;
        self.ast.var_statement(identifier, data_type, expr)
    }

    fn parse_compound_statement(&mut self) -> Option<&ASTStatement> {
        let start_brace = self.consume_expected(TokenKind::LeftBrace)?.clone();
        let mut statements: Vec<StmntId> = Vec::new();
        while self.current_token().kind != TokenKind::RightBrace
            && self.current_token().kind != TokenKind::Eof
        {
            println!("Help {:?}", self.current_token());
            statements.push(self.parse_statement()?.id);
        }
        let end_brace = self.consume_expected(TokenKind::RightBrace)?.clone();
        self.ast.compound(statements, start_brace, end_brace)
    }

    fn parse_function_statement(&mut self) -> Option<&ASTStatement> {
        self.consume_expected(TokenKind::Func)?;
        let identifier = self.consume_expected(TokenKind::Identifier)?.clone();
        self.consume_expected(TokenKind::LeftParen)?;

        if self.current_token().kind == TokenKind::Comma {
            self.diagnostics_colletion
                .borrow_mut()
                .report_unexpected_token(&TokenKind::Identifier, self.peek(1));
            self.consume();
            return None;
        }

        let mut arguments: Vec<FunctionArgumentDeclaration> = Vec::new();
        while self.current_token().kind != TokenKind::RightParen
            && self.current_token().kind != TokenKind::Eof
        {
            if self.current_token().kind == TokenKind::Comma {
                self.diagnostics_colletion
                    .borrow_mut()
                    .report_unexpected_token(&TokenKind::Identifier, self.current_token());
                self.consume();
                return None;
            }

            if self.current_token().kind == TokenKind::Identifier {
                let identifier = self.consume().clone();
                self.consume_expected(TokenKind::Colon)?;
                arguments.push(FunctionArgumentDeclaration {
                    identifier,
                    data_type: self.consume().clone(),
                });
            } else {
                self.diagnostics_colletion
                    .borrow_mut()
                    .report_unexpected_token(&TokenKind::Identifier, self.current_token());
                return None;
            }

            if self.current_token().kind == TokenKind::Comma
                && self.peek(1).kind == TokenKind::RightParen
            {
                self.consume_expected(TokenKind::RightParen)?;
                break;
            } else if self.current_token().kind == TokenKind::Comma {
                self.consume(); // Consume comma if present
            }
        }

        let right_paren = self.consume_expected(TokenKind::RightParen)?;

        // Return type is declared like `func foo() -> i32 {...}`
        let return_type = if self.current_token().kind == TokenKind::MinusRightAngleBracket {
            self.consume_expected(TokenKind::MinusRightAngleBracket)?;
            self.consume().clone()
        } else {
            Token {
                kind: TokenKind::Void,
                span: TextSpan::new(
                    right_paren.span.start,
                    right_paren.span.end,
                    "void".to_string(),
                ),
            }
        };

        let body = self.parse_compound_statement()?.id;

        self.ast.function(identifier, arguments, body, return_type)
    }

    fn consume_optional_else_statement(&mut self) -> Option<ASTElseStatement> {
        if self.current_token().kind != TokenKind::Else {
            return None;
        }
        let else_keyword = self.consume_expected(TokenKind::Else)?.clone();
        let else_branch = self.parse_statement()?.id;
        Some(ASTElseStatement {
            else_keyword: else_keyword,
            else_branch,
        })
    }

    fn consume_assignment_operator(&mut self) -> ASTBinaryOperator {
        let op = self.consume().clone();
        let kind = match op.kind {
            TokenKind::PlusEqual => ASTBinaryOperatorKind::Plus,
            TokenKind::MinusEqual => ASTBinaryOperatorKind::Minus,
            TokenKind::AstriskEqual => ASTBinaryOperatorKind::Multiply,
            TokenKind::SlashEqual => ASTBinaryOperatorKind::Divide,
            TokenKind::PipeEqual => ASTBinaryOperatorKind::BitwiseOR,
            TokenKind::AmpersandEqual => ASTBinaryOperatorKind::BitwiseAND,
            TokenKind::CaretEqual => ASTBinaryOperatorKind::BitwiseXOR,
            _ => todo!(),
        };
        ASTBinaryOperator { kind, token: op }
    }

    fn parse_if_statement(&mut self) -> Option<&ASTStatement> {
        let keyword = self.consume_expected(TokenKind::If)?.clone();
        let condition = self.parse_expression()?.id;
        let then_branch = self.parse_compound_statement()?.id;
        let else_branch = self.consume_optional_else_statement();

        self.ast
            .conditional(keyword, condition, then_branch, else_branch)
    }

    fn parse_while_loop_statement(&mut self) -> Option<&ASTStatement> {
        let keyword = self.consume_expected(TokenKind::While)?.clone();
        let condition = self.parse_expression()?.id;
        let body = self.parse_compound_statement()?.id;

        self.ast.while_loop(keyword, condition, body)
    }

    fn parse_for_loop_statement(&mut self) -> Option<&ASTStatement> {
        let keyword = self.consume_expected(TokenKind::For)?.clone();
        let loop_variable = self.consume_expected(TokenKind::Identifier)?.clone();
        self.consume_expected(TokenKind::In)?;
        let range_start = self.parse_expression()?.id;
        self.consume_expected(TokenKind::Dot)?;
        self.consume_expected(TokenKind::Dot)?;
        let range_end = self.parse_expression()?.id;

        let body = self.parse_compound_statement()?.id;

        self.ast
            .for_loop(keyword, loop_variable, (range_start, range_end), body)
    }

    fn parse_expression_statement(&mut self) -> Option<&ASTStatement> {
        let expr = self.parse_expression()?.id;
        self.consume_expected(TokenKind::SemiColon)?;
        self.ast.expression(expr)
    }

    fn parse_assignment_expression(&mut self) -> Option<&ASTExpression> {
        if self.current_token().kind == TokenKind::Identifier {
            if self.peek(1).kind == TokenKind::Equal {
                let var = self.consume().clone();
                self.consume_expected(TokenKind::Equal)?;
                let assignment = self.parse_binary_expression(0)?.id;
                return self.ast.assignment(var, assignment);
            }
            if self.peek(1).kind == TokenKind::PlusEqual
                || self.peek(1).kind == TokenKind::MinusEqual
                || self.peek(1).kind == TokenKind::AstriskEqual
                || self.peek(1).kind == TokenKind::SlashEqual
            {
                let var = self.consume().clone();
                let op = self.consume_assignment_operator();
                let assignment = self.parse_binary_expression(0)?.id;
                let og = self.ast.identifier(var.clone())?.id;
                let a = self.ast.binary(op, og, assignment)?.id;
                return self.ast.assignment(var.clone(), a);
            }
        }
        self.parse_binary_expression(0)
    }

    fn parse_expression(&mut self) -> Option<&ASTExpression> {
        self.parse_assignment_expression()
    }

    fn parse_arguments_list(&mut self) -> Option<Vec<ExprId>> {
        if self.current_token().kind == TokenKind::Comma {
            self.diagnostics_colletion
                .borrow_mut()
                .report_unexpected_token(&TokenKind::Identifier, self.peek(1));
            self.consume();
            return None;
        }

        let mut arguments: Vec<ExprId> = Vec::new();
        while self.current_token().kind != TokenKind::RightParen
            && self.current_token().kind != TokenKind::Eof
        {
            if self.current_token().kind == TokenKind::Comma {
                self.diagnostics_colletion
                    .borrow_mut()
                    .report_unexpected_token(&TokenKind::Identifier, self.current_token());
                self.consume();
                return None;
            }
            arguments.push(self.parse_expression()?.id);
            if self.current_token().kind == TokenKind::Comma
                && self.peek(1).kind == TokenKind::RightParen
            {
                self.consume_expected(TokenKind::RightParen);
                break;
            } else if self.current_token().kind == TokenKind::Comma {
                self.consume(); // Consume comma if present
            }
        }
        Some(arguments)
    }

    fn parse_function_call_expression(&mut self) -> Option<&ASTExpression> {
        let identifier = self.peek(-1).clone();
        self.consume();
        let arguments = self.parse_arguments_list()?;
        self.consume_expected(TokenKind::RightParen)?;
        self.ast.function_call(identifier.clone(), arguments)
    }

    fn parse_primary_expression(&mut self) -> Option<&ASTExpression> {
        let token = self.consume().clone();

        return match token.kind {
            TokenKind::Integer(i) => self.ast.integer(i),
            TokenKind::Floating(i) => self.ast.float(i),
            TokenKind::Identifier => {
                if self.current_token().kind == TokenKind::LeftParen {
                    self.parse_function_call_expression()
                } else if self.current_token().kind == TokenKind::Dot {
                    None
                    // self.consume();
                    // let member: &ASTExpression = self.parse_expression()?;
                    // match member.kind {
                    //     ASTExpressionKind::Variable(_) => {
                    //         // return self.ast.member_access(token.clone(), member.id.clone());
                    //     }
                    //     ASTExpressionKind::MemberAccess(_) => return None,
                    //     _ => return None,
                    // };
                } else if token.name() == "false" {
                    self.ast.boolean(false)
                } else if token.name() == "true" {
                    self.ast.boolean(true)
                } else {
                    self.ast.identifier(token.clone())
                }
            }

            TokenKind::LeftParen => {
                let expr = self.parse_binary_expression(0)?.id;
                let _found_token = self.consume_expected(TokenKind::RightParen)?;
                self.ast.parenthesized(expr)
            }
            TokenKind::Tilde | TokenKind::Minus | TokenKind::ExclemationMark => {
                self.parse_unary_expression()
            }
            _ => {
                self.diagnostics_colletion
                    .borrow_mut()
                    .report_expected_expression(&token);
                self.ast.error(token.span)
            }
        };
    }

    fn parse_unary_expression(&mut self) -> Option<&ASTExpression> {
        let operator = self.parse_unary_operator().unwrap();
        let expr = self.parse_primary_expression()?.id;
        // Some(ASTExpression::unary(operator, expr))
        self.ast.unary(operator, expr)
    }
    fn parse_binary_expression(&mut self, precedence: u8) -> Option<&ASTExpression> {
        let mut left = self.parse_primary_expression()?.id;

        while let Some(operator) = self.parse_binary_operator() {
            let operator_precedence = operator.precedence();
            if operator_precedence > precedence {
                self.consume();
                let right = self.parse_binary_expression(operator_precedence)?.id;
                left = self.ast.binary(operator, left, right)?.id;
            } else {
                break;
            }
        }
        Some(self.ast.query_expression(left))
    }

    fn parse_binary_operator(&mut self) -> Option<ASTBinaryOperator> {
        let token = self.current_token();
        let kind = match token.kind {
            TokenKind::Plus => Some(ASTBinaryOperatorKind::Plus),
            TokenKind::Minus => Some(ASTBinaryOperatorKind::Minus),
            TokenKind::Astrisk => Some(ASTBinaryOperatorKind::Multiply),
            TokenKind::Slash => Some(ASTBinaryOperatorKind::Divide),

            TokenKind::Pipe => Some(ASTBinaryOperatorKind::BitwiseOR),
            TokenKind::Ampersand => Some(ASTBinaryOperatorKind::BitwiseAND),
            TokenKind::Caret => Some(ASTBinaryOperatorKind::BitwiseXOR),
            TokenKind::EqualEqual => Some(ASTBinaryOperatorKind::EqualTo),
            TokenKind::ExclemationMarkEqual => Some(ASTBinaryOperatorKind::NotEqualTo),
            TokenKind::AmpersandAmpersand => Some(ASTBinaryOperatorKind::LogicAND),
            TokenKind::PipePipe => Some(ASTBinaryOperatorKind::LogicOR),
            TokenKind::RightAngleBracket => Some(ASTBinaryOperatorKind::GreaterThan),
            TokenKind::RightAngleBracketEqual => Some(ASTBinaryOperatorKind::GreaterThanOrEqual),
            TokenKind::LeftAngleBracket => Some(ASTBinaryOperatorKind::LessThan),
            TokenKind::LeftAngleBracketEqual => Some(ASTBinaryOperatorKind::LessThanOrEqual),
            _ => None,
        };
        kind.map(|kind| {
            return ASTBinaryOperator {
                kind,
                token: token.clone(),
            };
        })
    }

    fn parse_unary_operator(&mut self) -> Option<ASTUnaryOperator> {
        let token = self.current_token();
        let kind = match token.kind {
            TokenKind::Tilde => Some(ASTUnaryOperatorKind::BitwiseNOT),
            TokenKind::ExclemationMark => Some(ASTUnaryOperatorKind::LogicNot),
            TokenKind::Minus => Some(ASTUnaryOperatorKind::Minus),
            _ => None,
        };
        kind.map(|kind| {
            return ASTUnaryOperator {
                kind,
                token: token.clone(),
            };
        })
    }
}
