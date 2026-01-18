use lexer::{TextSpan, Token};
use printer::ASTTreePrinter;

pub mod lexer;
pub mod parser;
pub mod printer;
pub mod solver;
pub mod symbol_table;
pub mod symbol_table_builder;
pub mod type_checker;

pub type StmntId = usize;
pub type ExprId = usize;

pub struct Ast {
    statements: Vec<ASTStatement>,
    expressions: Vec<ASTExpression>,
    top_level_statements: Vec<StmntId>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
            expressions: Vec::new(),
            top_level_statements: Vec::new(),
        }
    }

    pub fn add_stmnt(&mut self, kind: ASTStatementKind) -> Option<&ASTStatement> {
        let id = self.statements.len();
        self.statements.push(ASTStatement::new(kind, id));
        self.statements.last()
    }
    pub fn add_expr(&mut self, kind: ASTExpressionKind) -> Option<&ASTExpression> {
        let id = self.expressions.len();
        self.expressions.push(ASTExpression::new(kind, id));
        self.expressions.last()
    }

    pub fn query_statement(&self, id: StmntId) -> &ASTStatement {
        self.statements.get(id).unwrap()
    }
    pub fn query_expression(&self, id: ExprId) -> &ASTExpression {
        self.expressions.get(id).unwrap()
    }

    pub fn add_statement(&mut self, statement: ASTStatement) {
        self.statements.push(statement);
    }

    // pub fn visit<T>(&self, printer: &mut dyn ASTVisitor<T>) {
    //     for statement in &self.statements {
    //         printer.visit_statement(statement);
    //     }
    // }

    pub fn visualize(&mut self) {
        let mut printer = ASTTreePrinter::new();
        let decoration = "=".repeat(80);
        println!("{decoration}\nAST:\n{decoration}");
        printer.create(self);
        println!("{decoration}\n");
    }

    // Statement insertions

    fn expression(&mut self, expr_id: ExprId) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::Expr(expr_id))
    }

    fn return_statement(&mut self, keyword: Token, expr: ExprId) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::Return(ASTReturnStatement {
            keyword,
            expr,
        }))
    }
    fn let_statement(
        &mut self,
        identifier: Token,
        data_type: Token,
        initializer: ExprId,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::Let(ASTLetStatement {
            identifier,
            data_type,
            initializer,
        }))
    }

    fn var_statement(
        &mut self,
        identifier: Token,
        data_type: Token,
        initializer: ExprId,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::Var(ASTVarStatement {
            identifier,
            data_type,
            initializer,
        }))
    }

    fn compound(
        &mut self,
        statements: Vec<StmntId>,
        start_brace: Token,
        end_brace: Token,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::Compound(ASTCompoundStatement {
            statements,
            start_brace,
            end_brace,
        }))
    }

    fn conditional(
        &mut self,
        keyword: Token,
        condition: ExprId,
        then_branch: StmntId,
        else_branch: Option<ASTElseStatement>,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::If(ASTIfStatement {
            keyword,
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn while_loop(
        &mut self,
        keyword: Token,
        condition: ExprId,
        body: StmntId,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::While(ASTWhileStatement {
            keyword,
            condition,
            body,
        }))
    }

    fn for_loop(
        &mut self,
        keyword: Token,
        loop_variable: Token,
        range: (ExprId, ExprId),
        body: StmntId,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::For(ASTForStatement {
            keyword,
            loop_variable,
            range,
            body,
        }))
    }

    fn function(
        &mut self,
        identifier: Token,
        arguments: Vec<FunctionArgumentDeclaration>,
        body: StmntId,
        return_type: Token,
    ) -> Option<&ASTStatement> {
        self.add_stmnt(ASTStatementKind::FuncDecl(ASTFunctionStatement {
            identifier,
            arguments,
            body,
            return_type,
        }))
    }

    // Expression helpers
    fn error(&mut self, span: TextSpan) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::Error(span))
    }

    fn integer(&mut self, i: i64) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::IntegerLiteral(i))
    }
    fn float(&mut self, f: f64) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::FloatingLiteral(f))
    }

    fn boolean(&mut self, b: bool) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::BooleanLiteral(b))
    }

    fn identifier(&mut self, token: Token) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::Variable(ASTVariableExpression {
            identifier: token,
        }))
    }

    fn assignment(&mut self, token: Token, expr_id: ExprId) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::Assignment(ASTAssignmentExpression {
            identifier: token,
            expr: expr_id,
        }))
    }

    fn unary(&mut self, operator: ASTUnaryOperator, expr_id: ExprId) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::Unary(ASTUnaryExpression {
            operator,
            expr: expr_id,
        }))
    }

    fn binary(
        &mut self,
        operator: ASTBinaryOperator,
        left_id: ExprId,
        right_id: ExprId,
    ) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::Binary(ASTBinaryExpression {
            operator,
            left: left_id,
            right: right_id,
        }))
    }

    fn parenthesized(&mut self, expr_id: ExprId) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::Parenthesized(
            ASTParenthesizedExpression { expr: expr_id },
        ))
    }

    fn function_call(
        &mut self,
        identifier: Token,
        arguments: Vec<ExprId>,
    ) -> Option<&ASTExpression> {
        self.add_expr(ASTExpressionKind::FunctionCall(ASTFunctionCallExpression {
            identifier,
            arguments,
        }))
    }
}

pub trait ASTVisitor<T> {
    fn do_visit_statement(&mut self, ast: &mut Ast, statement_id: StmntId) -> T {
        let statement = ast.query_statement(statement_id).clone();
        match &statement.kind {
            ASTStatementKind::Expr(expr) => self.visit_expression(ast, *expr),
            ASTStatementKind::Return(statement) => self.visit_return_statement(ast, statement),
            ASTStatementKind::FuncDecl(statement) => self.visit_function_statement(ast, statement),
            ASTStatementKind::Let(statement) => self.visit_let_statement(ast, statement),
            ASTStatementKind::Var(statement) => self.visit_var_statement(ast, statement),
            ASTStatementKind::Compound(statement) => self.visit_compound_statement(ast, statement),
            ASTStatementKind::If(statement) => self.visit_if_statement(ast, statement),
            ASTStatementKind::For(statement) => self.visit_for_loop_statement(ast, statement),
            ASTStatementKind::While(statement) => self.visit_while_loop_statement(ast, statement),
        }
    }

    fn do_visit_expression(&mut self, ast: &mut Ast, expr_id: ExprId) -> T {
        let expr = ast.query_expression(expr_id).clone();
        match &expr.kind {
            ASTExpressionKind::IntegerLiteral(i) => self.visit_integer(i),
            ASTExpressionKind::BooleanLiteral(b) => self.visit_boolean(b.clone()),
            ASTExpressionKind::FloatingLiteral(f) => self.visit_float(f),
            ASTExpressionKind::Variable(expr) => self.visit_variable_expression(ast, expr),
            ASTExpressionKind::StringLiteral(_) => todo!(),
            ASTExpressionKind::Unary(expr) => self.visit_unary_expression(ast, expr),
            ASTExpressionKind::Binary(expr) => self.visit_binary_expression(ast, expr),
            ASTExpressionKind::Parenthesized(expr) => {
                self.visit_parenthesised_expression(ast, expr)
            }
            ASTExpressionKind::FunctionCall(expr) => self.visit_function_call_expression(ast, expr),
            ASTExpressionKind::Assignment(expr) => self.visit_assignment_expression(ast, expr),
            ASTExpressionKind::Error(span) => self.visit_error(span),
        }
    }

    fn visit_statement(&mut self, ast: &mut Ast, statement: StmntId) -> T {
        return self.do_visit_statement(ast, statement);
    }

    fn visit_compound_statement(&mut self, ast: &mut Ast, statement: &ASTCompoundStatement) -> T;
    fn visit_return_statement(&mut self, ast: &mut Ast, statement: &ASTReturnStatement) -> T;
    fn visit_let_statement(&mut self, ast: &mut Ast, statement: &ASTLetStatement) -> T;
    fn visit_var_statement(&mut self, ast: &mut Ast, statement: &ASTVarStatement) -> T;
    fn visit_if_statement(&mut self, ast: &mut Ast, statement: &ASTIfStatement) -> T;
    fn visit_for_loop_statement(&mut self, ast: &mut Ast, statement: &ASTForStatement) -> T;
    fn visit_while_loop_statement(&mut self, ast: &mut Ast, statement: &ASTWhileStatement) -> T;
    fn visit_function_statement(&mut self, ast: &mut Ast, function: &ASTFunctionStatement) -> T;

    fn visit_expression(&mut self, ast: &mut Ast, expr_id: ExprId) -> T {
        return self.do_visit_expression(ast, expr_id);
    }

    fn visit_assignment_expression(&mut self, ast: &mut Ast, expr: &ASTAssignmentExpression) -> T;
    fn visit_function_call_expression(
        &mut self,
        ast: &mut Ast,
        expr: &ASTFunctionCallExpression,
    ) -> T;
    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast,
        unary_expr: &ASTUnaryExpression,
        expr: &ASTExpression,
    ) -> T;
    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast,
        binary_expr: &ASTBinaryExpression,
        expr: &ASTExpression,
    ) -> T;
    fn visit_parenthesised_expression(
        &mut self,
        ast: &mut Ast,
        expr: &ASTParenthesizedExpression,
    ) -> T;
    fn visit_binary_operator(&mut self, op: &ASTBinaryOperator) -> T;

    fn visit_error(&mut self, span: &TextSpan) -> T;
    fn visit_integer(&mut self, integer: &i64) -> T;
    fn visit_boolean(&mut self, boolean: bool) -> T;
    fn visit_float(&mut self, float: &f64) -> T;
}

#[derive(Clone)]
enum ASTStatementKind {
    Expr(ExprId),
    Let(ASTLetStatement),
    Var(ASTVarStatement),
    Return(ASTReturnStatement),
    Compound(ASTCompoundStatement),
    FuncDecl(ASTFunctionStatement),
    If(ASTIfStatement),
    While(ASTWhileStatement),
    For(ASTForStatement),
}

#[derive(Clone)]
pub struct ASTLetStatement {
    identifier: Token,
    data_type: Token,
    initializer: ExprId,
}

#[derive(Clone)]
pub struct ASTVarStatement {
    identifier: Token,
    data_type: Token,
    initializer: ExprId,
}

#[derive(Clone)]
pub struct ASTReturnStatement {
    keyword: Token,
    expr: ExprId,
}
#[derive(Clone)]
pub struct ASTCompoundStatement {
    statements: Vec<StmntId>,
    start_brace: Token,
    end_brace: Token,
}

#[derive(Clone)]
pub struct FunctionArgumentDeclaration {
    identifier: Token,
    data_type: Token,
}

#[derive(Clone)]
pub struct ASTFunctionStatement {
    identifier: Token,
    arguments: Vec<FunctionArgumentDeclaration>,
    body: StmntId,
    return_type: Token,
}

#[derive(Clone)]
pub struct ASTElseStatement {
    else_keyword: Token,
    else_branch: StmntId,
}
#[derive(Clone)]
pub struct ASTIfStatement {
    keyword: Token,
    condition: ExprId,
    then_branch: StmntId,
    else_branch: Option<ASTElseStatement>,
}

#[derive(Clone)]
pub struct ASTWhileStatement {
    keyword: Token,
    condition: ExprId,
    body: StmntId,
}

#[derive(Clone)]
pub struct ASTForStatement {
    keyword: Token,
    loop_variable: Token,
    range: (ExprId, ExprId),
    body: StmntId,
}

#[derive(Clone)]
pub struct ASTStatement {
    kind: ASTStatementKind,
    id: StmntId,
}

impl ASTStatement {
    fn new(kind: ASTStatementKind, id: StmntId) -> Self {
        Self { kind, id }
    }

    fn expression(expr_id: ExprId) -> Self {
        Self {
            kind: ASTStatementKind::Expr(expr_id),
            id: 0,
        }
    }

    fn return_statement(keyword: Token, expr: ExprId) -> Self {
        Self {
            kind: ASTStatementKind::Return(ASTReturnStatement { keyword, expr }),
            id: 0,
        }
    }
    fn let_statement(identifier: Token, data_type: Token, initializer: ExprId) -> Self {
        Self {
            kind: ASTStatementKind::Let(ASTLetStatement {
                identifier,
                data_type,
                initializer,
            }),
            id: 0,
        }
    }

    fn var_statement(identifier: Token, data_type: Token, initializer: ExprId) -> Self {
        Self {
            kind: ASTStatementKind::Var(ASTVarStatement {
                identifier,
                data_type,
                initializer,
            }),
            id: 0,
        }
    }

    fn compound(statements: Vec<StmntId>, start_brace: Token, end_brace: Token) -> Self {
        Self {
            kind: ASTStatementKind::Compound(ASTCompoundStatement {
                statements,
                start_brace,
                end_brace,
            }),
            id: 0,
        }
    }

    fn conditional(
        keyword: Token,
        condition: ExprId,
        then_branch: ASTStatement,
        else_branch: Option<ASTElseStatement>,
    ) -> Self {
        Self {
            kind: ASTStatementKind::If(ASTIfStatement {
                keyword,
                condition,
                then_branch: then_branch.id,
                else_branch,
            }),
            id: 0,
        }
    }

    fn while_loop(keyword: Token, condition: ExprId, body: ASTStatement) -> Self {
        Self {
            kind: ASTStatementKind::While(ASTWhileStatement {
                keyword,
                condition,
                body: body.id,
            }),
            id: 0,
        }
    }

    fn for_loop(
        keyword: Token,
        loop_variable: Token,
        range: (ExprId, ExprId),
        body: ASTStatement,
    ) -> Self {
        Self {
            kind: ASTStatementKind::For(ASTForStatement {
                keyword,
                loop_variable,
                range,
                body: body.id,
            }),
            id: 0,
        }
    }

    fn function(
        identifier: Token,
        arguments: Vec<FunctionArgumentDeclaration>,
        body: ASTStatement,
        return_type: Token,
    ) -> Self {
        Self {
            kind: ASTStatementKind::FuncDecl(ASTFunctionStatement {
                identifier,
                arguments,
                body: body.id,
                return_type,
            }),
            id: 0,
        }
    }
}

#[derive(Clone, PartialEq)]
enum ASTExpressionKind {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    FloatingLiteral(f64),
    StringLiteral(String),
    Unary(ASTUnaryExpression),
    Binary(ASTBinaryExpression),
    Parenthesized(ASTParenthesizedExpression),
    Variable(ASTVariableExpression),
    Assignment(ASTAssignmentExpression),
    FunctionCall(ASTFunctionCallExpression),
    Error(TextSpan),
}

#[derive(Clone, PartialEq)]
pub struct ASTExpression {
    kind: ASTExpressionKind,
    id: ExprId,
}

impl ASTExpression {
    fn new(kind: ASTExpressionKind, id: ExprId) -> Self {
        Self { kind, id }
    }
}

#[derive(Clone, PartialEq)]
enum ASTUnaryOperatorKind {
    Minus,
    BitwiseNOT,
    LogicNot,
}

#[derive(Clone, PartialEq)]
struct ASTUnaryOperator {
    kind: ASTUnaryOperatorKind,
    token: lexer::Token,
}
#[derive(Clone, PartialEq)]
pub struct ASTUnaryExpression {
    operator: ASTUnaryOperator,
    expr: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
enum ASTBinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    EqualTo,
    NotEqualTo,
    LogicAND,
    LogicOR,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    BitwiseOR,
    BitwiseAND,
    BitwiseXOR,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBinaryOperator {
    kind: ASTBinaryOperatorKind,
    token: lexer::Token,
}

impl ASTBinaryOperator {
    fn precedence(&self) -> u8 {
        match self.kind {
            ASTBinaryOperatorKind::Plus => 5,
            ASTBinaryOperatorKind::Minus => 5,
            ASTBinaryOperatorKind::Multiply => 6,
            ASTBinaryOperatorKind::Divide => 6,
            ASTBinaryOperatorKind::EqualTo => 1,
            ASTBinaryOperatorKind::NotEqualTo => 1,
            ASTBinaryOperatorKind::LogicAND => 1,
            ASTBinaryOperatorKind::LogicOR => 1,
            ASTBinaryOperatorKind::GreaterThan => 1,
            ASTBinaryOperatorKind::GreaterThanOrEqual => 1,
            ASTBinaryOperatorKind::LessThan => 1,
            ASTBinaryOperatorKind::LessThanOrEqual => 1,
            ASTBinaryOperatorKind::BitwiseOR => 1,
            ASTBinaryOperatorKind::BitwiseAND => 1,
            ASTBinaryOperatorKind::BitwiseXOR => 1,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct ASTBinaryExpression {
    operator: ASTBinaryOperator,
    left: ExprId,
    right: ExprId,
}

#[derive(Clone, PartialEq)]
pub struct ASTParenthesizedExpression {
    expr: ExprId,
}

#[derive(Clone, PartialEq)]
pub struct ASTVariableExpression {
    identifier: Token,
}

#[derive(Clone, PartialEq)]
pub struct ASTAssignmentExpression {
    identifier: Token,
    expr: ExprId,
}

impl ASTVariableExpression {
    pub fn identifier(&self) -> &str {
        &self.identifier.span.literal
    }
}

#[derive(Clone, PartialEq)]
pub struct ASTFunctionCallExpression {
    identifier: Token,
    arguments: Vec<ExprId>,
}

impl ASTFunctionCallExpression {
    pub fn identifier(&self) -> &str {
        &self.identifier.span.literal
    }
}

//
// #[cfg(test)]
// mod test {
//     use crate::compilation_unit::CompilationUnit;
//
//     use super::lexer::TokenKind;
//     use super::ASTVisitor;
//     use super::Ast;
//
//     #[derive(Debug, PartialEq)]
//     enum TestASTNode {
//         Floating(f64),
//         Integer(i64),
//         Boolean(bool),
//         Variable(String),
//         Let(String, TokenKind),
//         Var(String, TokenKind),
//         Assign(String),
//         If,
//         While,
//         For(String),
//         Return,
//         FuncDecl(Vec<(String, TokenKind)>),
//         BinaryExpr(TokenKind),
//         UnaryExpr(TokenKind),
//         ParenExpr,
//         FunctionCall(String),
//     }
//     struct ASTVerifier {
//         actual: Vec<TestASTNode>,
//         expected: Vec<TestASTNode>,
//     }
//
//     impl ASTVerifier {
//         pub fn new(input: &str, expected_ast: Vec<TestASTNode>) -> Self {
//             let compilation_unit = CompilationUnit::compile(input);
//             assert!(compilation_unit.is_ok());
//             let mut verifier = ASTVerifier {
//                 actual: Vec::new(),
//                 expected: expected_ast,
//             };
//
//             match compilation_unit {
//                 Ok(c) => verifier.flatten_ast(&c.ast),
//                 Err(_) => (),
//             };
//             verifier
//         }
//
//         fn flatten_ast(&mut self, ast: &Ast) {
//             ast.visit(&mut *self);
//         }
//
//         pub fn verify(&self) {
//             assert_eq!(
//                 self.expected.len(),
//                 self.actual.len(),
//                 "Expected {} nodes but has {} ",
//                 self.expected.len(),
//                 self.actual.len()
//             );
//
//             for (ac, ex) in self.actual.iter().zip(self.expected.iter()) {
//                 assert_eq!(
//                     ac, ex,
//                     "Node do not match. Expected {:?} but found {:?}",
//                     ex, ac
//                 )
//             }
//         }
//     }
//
//     impl ASTVisitor<()> for ASTVerifier {
//         fn visit_return_statement(&mut self, ast: &mut Ast,  statement: &super::ASTReturnStatement) {
//             self.actual.push(TestASTNode::Return);
//             self.visit_expression(&statement.expr);
//         }
//
//         fn visit_let_statement(&mut self, ast: &mut Ast,  statement: &super::ASTLetStatement) {
//             self.actual.push(TestASTNode::Let(
//                 statement.identifier.span.literal.clone(),
//                 statement.data_type.kind.clone(),
//             ));
//             self.visit_expression(&statement.initializer);
//         }
//
//         fn visit_var_statement(&mut self, ast: &mut Ast,  statement: &super::ASTVarStatement) {
//             self.actual.push(TestASTNode::Var(
//                 statement.identifier.span.literal.clone(),
//                 statement.data_type.kind.clone(),
//             ));
//             self.visit_expression(&statement.initializer);
//         }
//
//         fn visit_if_statement(&mut self, ast: &mut Ast,  statement: &super::ASTIfStatement) {
//             self.actual.push(TestASTNode::If);
//             self.visit_expression(&statement.condition);
//             if let super::ASTStatementKind::Compound(body) = &statement.then_branch.kind {
//                 self.visit_compound_statement(body);
//             }
//             if let Some(else_branch) = &statement.else_branch {
//                 self.visit_statement(&else_branch.else_branch);
//             }
//         }
//
//         fn visit_for_loop_statement(&mut self, ast: &mut Ast,  statement: &super::ASTForStatement) {
//             self.actual.push(TestASTNode::For(
//                 statement.loop_variable.span.literal.clone(),
//             ));
//             self.visit_expression(&statement.range.0);
//             self.visit_expression(&statement.range.1);
//             if let super::ASTStatementKind::Compound(body) = &statement.body.kind {
//                 self.visit_compound_statement(body);
//             }
//         }
//
//         fn visit_while_loop_statement(&mut self, ast: &mut Ast,  statement: &super::ASTWhileStatement) {
//             self.actual.push(TestASTNode::While);
//             self.visit_expression(&statement.condition);
//             if let super::ASTStatementKind::Compound(body) = &statement.body.kind {
//                 self.visit_compound_statement(body);
//             }
//         }
//
//         fn visit_function_statement(&mut self, ast: &mut Ast,  function: &super::ASTFunctionStatement) {
//             let mut args: Vec<(String, TokenKind)> = Vec::new();
//             args.push((
//                 function.identifier.span.literal.clone(),
//                 function.return_type.kind.clone(),
//             ));
//             for arg in function.arguments.iter() {
//                 args.push((
//                     arg.identifier.span.literal.clone(),
//                     arg.data_type.kind.clone(),
//                 ));
//             }
//
//             self.actual.push(TestASTNode::FuncDecl(args));
//
//             if let super::ASTStatementKind::Compound(statement) = &function.body.kind {
//                 self.visit_compound_statement(statement);
//             }
//         }
//
//         fn visit_assignment_expression(&mut self, ast: &mut Ast,  expr: &super::ASTAssignmentExpression) {
//             self.actual
//                 .push(TestASTNode::Assign(expr.identifier.span.literal.clone()));
//             self.visit_expression(&expr.expr);
//         }
//
//         fn visit_function_call_expression(&mut self, ast: &mut Ast,  expr: &super::ASTFunctionCallExpression) {
//             self.actual.push(TestASTNode::FunctionCall(
//                 expr.identifier.span.literal.clone(),
//             ));
//             for arg in expr.arguments.iter() {
//                 self.visit_expression(arg);
//             }
//         }
//
//         fn visit_variable_expression(&mut self, ast: &mut Ast,  expr: &super::ASTVariableExpression) {
//             self.actual
//                 .push(TestASTNode::Variable(expr.identifier.span.literal.clone()));
//         }
//
//         fn visit_unary_expression(&mut self, ast: &mut Ast,  expr: &super::ASTUnaryExpression) {
//             self.actual
//                 .push(TestASTNode::UnaryExpr(expr.operator.token.kind.clone()));
//         }
//
//         fn visit_binary_expression(&mut self, ast: &mut Ast,  expr: &super::ASTBinaryExpression) {
//             self.actual
//                 .push(TestASTNode::BinaryExpr(expr.operator.token.kind.clone()));
//             self.visit_expression(&expr.left);
//             self.visit_expression(&expr.right);
//         }
//
//         fn visit_parenthesised_expression(&mut self, ast: &mut Ast,  expr: &super::ASTParenthesizedExpression) {
//             self.actual.push(TestASTNode::ParenExpr);
//             self.visit_expression(&expr.expr);
//         }
//
//         fn visit_binary_operator(&mut self, ast: &mut Ast,  op: &super::ASTBinaryOperator) {}
//
//         fn visit_integer(&mut self, ast: &mut Ast,  integer: &i64) {
//             self.actual.push(TestASTNode::Integer(integer.clone()));
//         }
//
//         fn visit_boolean(&mut self, boolean: bool) {
//             self.actual.push(TestASTNode::Boolean(boolean));
//         }
//
//         fn visit_float(&mut self, float: &f64) {
//             self.actual.push(TestASTNode::Floating(float.clone()));
//         }
//
//         fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) -> () {
//             for stmnt in &statement.statements {
//                 self.visit_statement(&stmnt);
//             }
//         }
//
//         fn visit_error(&mut self, span: &super::lexer::TextSpan) -> () {
//             todo!()
//         }
//     }
//
//     #[test]
//     fn should_parse_let_statement() {
//         let input = "let a: u8 = 10;";
//         let expected_ast = vec![
//             TestASTNode::Let("a".to_string(), TokenKind::U8),
//             TestASTNode::Integer(10),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_return_statement() {
//         let input = "\
//         func main() -> i32 {
//             let a: i32 = 7;
//             return a + 10;
//         }
//         ";
//         let expected_ast = vec![
//             TestASTNode::FuncDecl(vec![("main".to_string(), TokenKind::I32)]),
//             TestASTNode::Let("a".to_string(), TokenKind::I32),
//             TestASTNode::Integer(7),
//             TestASTNode::Return,
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::Variable("a".to_string()),
//             TestASTNode::Integer(10),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_simple_binary_addition_statement() {
//         let input = "10 + 3.1415;";
//         let expected_ast = vec![
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::Integer(10),
//             TestASTNode::Floating(3.1415),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_complex_binary_statement() {
//         let input = "let a: f64 = (7.2 - 10) / 2 + 3.1415 * 8;";
//         let expected_ast = vec![
//             TestASTNode::Let("a".to_string(), TokenKind::F64),
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::BinaryExpr(TokenKind::Slash),
//             TestASTNode::ParenExpr,
//             TestASTNode::BinaryExpr(TokenKind::Minus),
//             TestASTNode::Floating(7.2),
//             TestASTNode::Integer(10),
//             TestASTNode::Integer(2),
//             TestASTNode::BinaryExpr(TokenKind::Astrisk),
//             TestASTNode::Floating(3.1415),
//             TestASTNode::Integer(8),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_function_declaration() {
//         let input = "func f(a: u8, b: i16, c: u64) -> u64 { return a + b + c; }";
//         let expected_ast = vec![
//             TestASTNode::FuncDecl(vec![
//                 ("f".to_string(), TokenKind::U64), // function name
//                 ("a".to_string(), TokenKind::U8),
//                 ("b".to_string(), TokenKind::I16),
//                 ("c".to_string(), TokenKind::U64),
//             ]),
//             TestASTNode::Return,
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::Variable("a".to_string()),
//             TestASTNode::Variable("b".to_string()),
//             TestASTNode::Variable("c".to_string()),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_function_call() {
//         let input = "\
//         func f(a: u8, b: i16, c: u64) -> u64 { return a + b + c; }
//         f(1, 2, 6);
//         ";
//         let expected_ast = vec![
//             TestASTNode::FuncDecl(vec![
//                 ("f".to_string(), TokenKind::U64), // function name
//                 ("a".to_string(), TokenKind::U8),
//                 ("b".to_string(), TokenKind::I16),
//                 ("c".to_string(), TokenKind::U64),
//             ]),
//             TestASTNode::Return,
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::BinaryExpr(TokenKind::Plus),
//             TestASTNode::Variable("a".to_string()),
//             TestASTNode::Variable("b".to_string()),
//             TestASTNode::Variable("c".to_string()),
//             TestASTNode::FunctionCall("f".to_string()),
//             TestASTNode::Integer(1),
//             TestASTNode::Integer(2),
//             TestASTNode::Integer(6),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_for_loop() {
//         let input = "\
//         func main() {
//             var a: u32 = 0;
//             for i in 0..10 {
//                 a += i;
//             }
//         }
//         ";
//         let expected_ast = vec![
//             TestASTNode::FuncDecl(vec![("main".to_string(), TokenKind::Void)]),
//             TestASTNode::Var("a".to_string(), TokenKind::U32),
//             TestASTNode::Integer(0),
//             TestASTNode::For("i".to_string()),
//             TestASTNode::Integer(0),
//             TestASTNode::Integer(10),
//             TestASTNode::Assign("a".to_string()),
//             TestASTNode::BinaryExpr(TokenKind::PlusEqual),
//             TestASTNode::Variable("a".to_string()),
//             TestASTNode::Variable("i".to_string()),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
//
//     #[test]
//     fn should_parse_while_loop() {
//         let input = "\
//         func main() {
//             var a: i32 = 5;
//             while a > 0 {
//                 a -= 1;
//             }
//         }
//         ";
//         let expected_ast = vec![
//             TestASTNode::FuncDecl(vec![("main".to_string(), TokenKind::Void)]),
//             TestASTNode::Var("a".to_string(), TokenKind::I32),
//             TestASTNode::Integer(5),
//             TestASTNode::While,
//             TestASTNode::BinaryExpr(TokenKind::RightAngleBracket),
//             TestASTNode::Variable("a".to_string()),
//             TestASTNode::Integer(0),
//             TestASTNode::Assign("a".to_string()),
//             TestASTNode::BinaryExpr(TokenKind::MinusEqual),
//             TestASTNode::Variable("a".to_string()),
//             TestASTNode::Integer(1),
//         ];
//
//         let verifier = ASTVerifier::new(input, expected_ast);
//         verifier.verify();
//     }
// }
