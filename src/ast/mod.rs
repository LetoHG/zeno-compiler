use lexer::{TextSpan, Token};
use printer::ASTTreePrinter;

pub mod lexer;
pub mod parser;
pub mod printer;
pub mod solver;
pub mod symbol_checker;
pub mod symbol_table;
pub mod type_checker;

pub struct Ast {
    statements: Vec<ASTStatement>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn add_statement(&mut self, statement: ASTStatement) {
        self.statements.push(statement);
    }

    pub fn visit<T>(&self, printer: &mut dyn ASTVisitor<T>) {
        for statement in &self.statements {
            printer.visit_statement(statement);
        }
    }

    pub fn visualize(&self) {
        let mut printer = ASTTreePrinter::new();
        let decoration = "=".repeat(80);
        println!("{decoration}\nAST:\n{decoration}");
        self.visit(&mut printer);
        println!("{decoration}\n");
    }
}

pub trait ASTVisitor<T> {
    fn do_visit_statement(&mut self, statement: &ASTStatement) -> T {
        match &statement.kind {
            ASTStatementKind::Expr(expr) => self.visit_expression(expr),
            ASTStatementKind::Return(statement) => self.visit_return_statement(statement),
            ASTStatementKind::FuncDecl(statement) => self.visit_function_statement(statement),
            ASTStatementKind::Let(statement) => self.visit_let_statement(statement),
            ASTStatementKind::Var(statement) => self.visit_var_statement(statement),
            ASTStatementKind::Compound(statement) => self.visit_compound_statement(statement),
            ASTStatementKind::If(statement) => self.visit_if_statement(statement),
            ASTStatementKind::For(statement) => self.visit_for_loop_statement(statement),
            ASTStatementKind::While(statement) => self.visit_while_loop_statement(statement),
        }
    }

    fn do_visit_expression(&mut self, expr: &ASTExpression) -> T {
        match &expr.kind {
            ASTExpressionKind::IntegerLiteral(i) => self.visit_integer(i),
            ASTExpressionKind::BooleanLiteral(b) => self.visit_boolean(b.clone()),
            ASTExpressionKind::FloatingLiteral(f) => self.visit_float(f),
            ASTExpressionKind::Variable(expr) => self.visit_variable_expression(expr),
            ASTExpressionKind::StringLiteral(_) => todo!(),
            ASTExpressionKind::Unary(expr) => self.visit_unary_expression(expr),
            ASTExpressionKind::Binary(expr) => self.visit_binary_expression(expr),
            ASTExpressionKind::Parenthesized(expr) => self.visit_parenthesised_expression(expr),
            ASTExpressionKind::FunctionCall(expr) => self.visit_function_call_expression(expr),
            ASTExpressionKind::Assignment(expr) => self.visit_assignment_expression(expr),
            ASTExpressionKind::Error(span) => self.visit_error(span),
        }
    }

    fn visit_statement(&mut self, statement: &ASTStatement) -> T {
        return self.do_visit_statement(statement);
    }
    fn visit_compound_statement(&mut self, statement: &ASTCompoundStatement) -> T;
    // {
    //     for statement in statement.statements.iter() {
    //         self.visit_statement(statement);
    //     }
    //     None
    // }

    fn visit_return_statement(&mut self, statement: &ASTReturnStatement) -> T;

    fn visit_let_statement(&mut self, statement: &ASTLetStatement) -> T;
    fn visit_var_statement(&mut self, statement: &ASTVarStatement) -> T;

    fn visit_if_statement(&mut self, statement: &ASTIfStatement) -> T;
    fn visit_for_loop_statement(&mut self, statement: &ASTForStatement) -> T;
    fn visit_while_loop_statement(&mut self, statement: &ASTWhileStatement) -> T;

    fn visit_function_statement(&mut self, function: &ASTFunctionStatement) -> T;
    // {
    //     if let ASTStatementKind::Compound(statement) = &function.body.kind {
    //         self.visit_compound_statement(statement);
    //     }
    //     None
    // }

    fn visit_expression(&mut self, expr: &ASTExpression) -> T {
        return self.do_visit_expression(expr);
    }

    fn visit_assignment_expression(&mut self, expr: &ASTAssignmentExpression) -> T;
    fn visit_function_call_expression(&mut self, expr: &ASTFunctionCallExpression) -> T;
    fn visit_variable_expression(&mut self, expr: &ASTVariableExpression) -> T;

    fn visit_unary_expression(&mut self, expr: &ASTUnaryExpression) -> T;
    fn visit_binary_expression(&mut self, expr: &ASTBinaryExpression) -> T;
    fn visit_parenthesised_expression(&mut self, expr: &ASTParenthesizedExpression) -> T;
    fn visit_binary_operator(&mut self, op: &ASTBinaryOperator) -> T;

    fn visit_error(&mut self, span: &TextSpan) -> T;
    fn visit_integer(&mut self, integer: &i64) -> T;
    fn visit_boolean(&mut self, boolean: bool) -> T;
    fn visit_float(&mut self, float: &f64) -> T;
}

#[derive(Clone)]
enum ASTStatementKind {
    Expr(ASTExpression),
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
    initializer: ASTExpression,
}

#[derive(Clone)]
pub struct ASTVarStatement {
    identifier: Token,
    data_type: Token,
    initializer: ASTExpression,
}

#[derive(Clone)]
pub struct ASTReturnStatement {
    keyword: Token,
    expr: ASTExpression,
}
#[derive(Clone)]
pub struct ASTCompoundStatement {
    statements: Vec<ASTStatement>,
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
    body: Box<ASTStatement>,
    return_type: Token,
}

#[derive(Clone)]
pub struct ASTElseStatement {
    else_keyword: Token,
    else_branch: Box<ASTStatement>,
}
#[derive(Clone)]
pub struct ASTIfStatement {
    keyword: Token,
    condition: ASTExpression,
    then_branch: Box<ASTStatement>,
    else_branch: Option<ASTElseStatement>,
}

#[derive(Clone)]
pub struct ASTWhileStatement {
    keyword: Token,
    condition: ASTExpression,
    body: Box<ASTStatement>,
}

#[derive(Clone)]
pub struct ASTForStatement {
    keyword: Token,
    loop_variable: Token,
    range: (ASTExpression, ASTExpression),
    body: Box<ASTStatement>,
}

#[derive(Clone)]
pub struct ASTStatement {
    kind: ASTStatementKind,
}

impl ASTStatement {
    fn new(kind: ASTStatementKind) -> Self {
        Self { kind }
    }

    fn expression(expr: ASTExpression) -> Self {
        Self {
            kind: ASTStatementKind::Expr(expr),
        }
    }

    fn return_statement(keyword: Token, expr: ASTExpression) -> Self {
        Self {
            kind: ASTStatementKind::Return(ASTReturnStatement { keyword, expr }),
        }
    }
    fn let_statement(identifier: Token, data_type: Token, initializer: ASTExpression) -> Self {
        Self {
            kind: ASTStatementKind::Let(ASTLetStatement {
                identifier,
                data_type,
                initializer,
            }),
        }
    }

    fn var_statement(identifier: Token, data_type: Token, initializer: ASTExpression) -> Self {
        Self {
            kind: ASTStatementKind::Var(ASTVarStatement {
                identifier,
                data_type,
                initializer,
            }),
        }
    }

    fn compound(statements: Vec<ASTStatement>, start_brace: Token, end_brace: Token) -> Self {
        Self {
            kind: ASTStatementKind::Compound(ASTCompoundStatement {
                statements,
                start_brace,
                end_brace,
            }),
        }
    }

    fn conditional(
        keyword: Token,
        condition: ASTExpression,
        then_branch: ASTStatement,
        else_branch: Option<ASTElseStatement>,
    ) -> Self {
        Self {
            kind: ASTStatementKind::If(ASTIfStatement {
                keyword,
                condition,
                then_branch: Box::new(then_branch),
                else_branch,
            }),
        }
    }

    fn while_loop(keyword: Token, condition: ASTExpression, body: ASTStatement) -> Self {
        Self {
            kind: ASTStatementKind::While(ASTWhileStatement {
                keyword,
                condition,
                body: Box::new(body),
            }),
        }
    }

    fn for_loop(
        keyword: Token,
        loop_variable: Token,
        range: (ASTExpression, ASTExpression),
        body: ASTStatement,
    ) -> Self {
        Self {
            kind: ASTStatementKind::For(ASTForStatement {
                keyword,
                loop_variable,
                range,
                body: Box::new(body),
            }),
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
                body: Box::new(body),
                return_type,
            }),
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
}

impl ASTExpression {
    fn new(kind: ASTExpressionKind) -> Self {
        Self { kind }
    }
    fn error(span: TextSpan) -> Self {
        Self {
            kind: ASTExpressionKind::Error(span),
        }
    }

    fn integer(i: i64) -> Self {
        Self {
            kind: ASTExpressionKind::IntegerLiteral(i),
        }
    }
    fn float(f: f64) -> Self {
        Self {
            kind: ASTExpressionKind::FloatingLiteral(f),
        }
    }

    fn boolean(b: bool) -> Self {
        Self {
            kind: ASTExpressionKind::BooleanLiteral(b),
        }
    }

    fn identifier(token: Token) -> Self {
        Self {
            kind: ASTExpressionKind::Variable(ASTVariableExpression { identifier: token }),
        }
    }

    fn assignment(token: Token, expr: ASTExpression) -> Self {
        Self {
            kind: ASTExpressionKind::Assignment(ASTAssignmentExpression {
                identifier: token,
                expr: Box::new(expr),
            }),
        }
    }

    fn unary(operator: ASTUnaryOperator, expr: ASTExpression) -> Self {
        Self {
            kind: ASTExpressionKind::Unary(ASTUnaryExpression {
                operator,
                expr: Box::new(expr),
            }),
        }
    }

    fn binary(operator: ASTBinaryOperator, left: ASTExpression, right: ASTExpression) -> Self {
        Self {
            kind: ASTExpressionKind::Binary(ASTBinaryExpression {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }),
        }
    }

    fn parenthesized(expr: ASTExpression) -> Self {
        Self {
            kind: ASTExpressionKind::Parenthesized(ASTParenthesizedExpression {
                expr: Box::new(expr),
            }),
        }
    }

    fn function_call(identifier: Token, arguments: Vec<ASTExpression>) -> Self {
        Self {
            kind: ASTExpressionKind::FunctionCall(ASTFunctionCallExpression {
                identifier,
                arguments,
            }),
        }
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
    expr: Box<ASTExpression>,
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
    left: Box<ASTExpression>,
    right: Box<ASTExpression>,
}

#[derive(Clone, PartialEq)]
pub struct ASTParenthesizedExpression {
    expr: Box<ASTExpression>,
}

#[derive(Clone, PartialEq)]
pub struct ASTVariableExpression {
    identifier: Token,
}

#[derive(Clone, PartialEq)]
pub struct ASTAssignmentExpression {
    identifier: Token,
    expr: Box<ASTExpression>,
}

impl ASTVariableExpression {
    pub fn identifier(&self) -> &str {
        &self.identifier.span.literal
    }
}

#[derive(Clone, PartialEq)]
pub struct ASTFunctionCallExpression {
    identifier: Token,
    arguments: Vec<ASTExpression>,
}

impl ASTFunctionCallExpression {
    pub fn identifier(&self) -> &str {
        &self.identifier.span.literal
    }
}

#[cfg(test)]
mod test {
    use crate::compilation_unit::CompilationUnit;

    use super::lexer::TokenKind;
    use super::ASTVisitor;
    use super::Ast;

    #[derive(Debug, PartialEq)]
    enum TestASTNode {
        Floating(f64),
        Integer(i64),
        Boolean(bool),
        Variable(String),
        Let(String, TokenKind),
        Var(String, TokenKind),
        Assign(String),
        If,
        While,
        For(String),
        Return,
        FuncDecl(Vec<(String, TokenKind)>),
        BinaryExpr(TokenKind),
        UnaryExpr(TokenKind),
        ParenExpr,
        FunctionCall(String),
    }
    struct ASTVerifier {
        actual: Vec<TestASTNode>,
        expected: Vec<TestASTNode>,
    }

    impl ASTVerifier {
        pub fn new(input: &str, expected_ast: Vec<TestASTNode>) -> Self {
            let compilation_unit = CompilationUnit::compile(input);
            assert!(compilation_unit.is_ok());
            let mut verifier = ASTVerifier {
                actual: Vec::new(),
                expected: expected_ast,
            };

            match compilation_unit {
                Ok(c) => verifier.flatten_ast(&c.ast),
                Err(_) => (),
            };
            verifier
        }

        fn flatten_ast(&mut self, ast: &Ast) {
            ast.visit(&mut *self);
        }

        pub fn verify(&self) {
            assert_eq!(
                self.expected.len(),
                self.actual.len(),
                "Expected {} nodes but has {} ",
                self.expected.len(),
                self.actual.len()
            );

            for (ac, ex) in self.actual.iter().zip(self.expected.iter()) {
                assert_eq!(
                    ac, ex,
                    "Node do not match. Expected {:?} but found {:?}",
                    ex, ac
                )
            }
        }
    }

    impl ASTVisitor<()> for ASTVerifier {
        fn visit_return_statement(&mut self, statement: &super::ASTReturnStatement) {
            self.actual.push(TestASTNode::Return);
            self.visit_expression(&statement.expr);
        }

        fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) {
            self.actual.push(TestASTNode::Let(
                statement.identifier.span.literal.clone(),
                statement.data_type.kind.clone(),
            ));
            self.visit_expression(&statement.initializer);
        }

        fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) {
            self.actual.push(TestASTNode::Var(
                statement.identifier.span.literal.clone(),
                statement.data_type.kind.clone(),
            ));
            self.visit_expression(&statement.initializer);
        }

        fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) {
            self.actual.push(TestASTNode::If);
            self.visit_expression(&statement.condition);
            if let super::ASTStatementKind::Compound(body) = &statement.then_branch.kind {
                self.visit_compound_statement(body);
            }
            if let Some(else_branch) = &statement.else_branch {
                self.visit_statement(&else_branch.else_branch);
            }
        }

        fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) {
            self.actual.push(TestASTNode::For(
                statement.loop_variable.span.literal.clone(),
            ));
            self.visit_expression(&statement.range.0);
            self.visit_expression(&statement.range.1);
            if let super::ASTStatementKind::Compound(body) = &statement.body.kind {
                self.visit_compound_statement(body);
            }
        }

        fn visit_while_loop_statement(&mut self, statement: &super::ASTWhileStatement) {
            self.actual.push(TestASTNode::While);
            self.visit_expression(&statement.condition);
            if let super::ASTStatementKind::Compound(body) = &statement.body.kind {
                self.visit_compound_statement(body);
            }
        }

        fn visit_function_statement(&mut self, function: &super::ASTFunctionStatement) {
            let mut args: Vec<(String, TokenKind)> = Vec::new();
            args.push((
                function.identifier.span.literal.clone(),
                function.return_type.kind.clone(),
            ));
            for arg in function.arguments.iter() {
                args.push((
                    arg.identifier.span.literal.clone(),
                    arg.data_type.kind.clone(),
                ));
            }

            self.actual.push(TestASTNode::FuncDecl(args));

            if let super::ASTStatementKind::Compound(statement) = &function.body.kind {
                self.visit_compound_statement(statement);
            }
        }

        fn visit_assignment_expression(&mut self, expr: &super::ASTAssignmentExpression) {
            self.actual
                .push(TestASTNode::Assign(expr.identifier.span.literal.clone()));
            self.visit_expression(&expr.expr);
        }

        fn visit_function_call_expression(&mut self, expr: &super::ASTFunctionCallExpression) {
            self.actual.push(TestASTNode::FunctionCall(
                expr.identifier.span.literal.clone(),
            ));
            for arg in expr.arguments.iter() {
                self.visit_expression(arg);
            }
        }

        fn visit_variable_expression(&mut self, expr: &super::ASTVariableExpression) {
            self.actual
                .push(TestASTNode::Variable(expr.identifier.span.literal.clone()));
        }

        fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) {
            self.actual
                .push(TestASTNode::UnaryExpr(expr.operator.token.kind.clone()));
        }

        fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) {
            self.actual
                .push(TestASTNode::BinaryExpr(expr.operator.token.kind.clone()));
            self.visit_expression(&expr.left);
            self.visit_expression(&expr.right);
        }

        fn visit_parenthesised_expression(&mut self, expr: &super::ASTParenthesizedExpression) {
            self.actual.push(TestASTNode::ParenExpr);
            self.visit_expression(&expr.expr);
        }

        fn visit_binary_operator(&mut self, op: &super::ASTBinaryOperator) {}

        fn visit_integer(&mut self, integer: &i64) {
            self.actual.push(TestASTNode::Integer(integer.clone()));
        }

        fn visit_boolean(&mut self, boolean: bool) {
            self.actual.push(TestASTNode::Boolean(boolean));
        }

        fn visit_float(&mut self, float: &f64) {
            self.actual.push(TestASTNode::Floating(float.clone()));
        }

        fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) -> () {
            for stmnt in &statement.statements {
                self.visit_statement(&stmnt);
            }
        }

        fn visit_error(&mut self, span: &super::lexer::TextSpan) -> () {
            todo!()
        }
    }

    #[test]
    fn should_parse_let_statement() {
        let input = "let a: u8 = 10;";
        let expected_ast = vec![
            TestASTNode::Let("a".to_string(), TokenKind::U8),
            TestASTNode::Integer(10),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_return_statement() {
        let input = "\
        func main() -> i32 {
            let a: i32 = 7;
            return a + 10;
        }
        ";
        let expected_ast = vec![
            TestASTNode::FuncDecl(vec![("main".to_string(), TokenKind::I32)]),
            TestASTNode::Let("a".to_string(), TokenKind::I32),
            TestASTNode::Integer(7),
            TestASTNode::Return,
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Integer(10),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_simple_binary_addition_statement() {
        let input = "10 + 3.1415;";
        let expected_ast = vec![
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::Integer(10),
            TestASTNode::Floating(3.1415),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_complex_binary_statement() {
        let input = "let a: f64 = (7.2 - 10) / 2 + 3.1415 * 8;";
        let expected_ast = vec![
            TestASTNode::Let("a".to_string(), TokenKind::F64),
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::BinaryExpr(TokenKind::Slash),
            TestASTNode::ParenExpr,
            TestASTNode::BinaryExpr(TokenKind::Minus),
            TestASTNode::Floating(7.2),
            TestASTNode::Integer(10),
            TestASTNode::Integer(2),
            TestASTNode::BinaryExpr(TokenKind::Astrisk),
            TestASTNode::Floating(3.1415),
            TestASTNode::Integer(8),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_function_declaration() {
        let input = "func f(a: u8, b: i16, c: u64) -> u64 { return a + b + c; }";
        let expected_ast = vec![
            TestASTNode::FuncDecl(vec![
                ("f".to_string(), TokenKind::U64), // function name
                ("a".to_string(), TokenKind::U8),
                ("b".to_string(), TokenKind::I16),
                ("c".to_string(), TokenKind::U64),
            ]),
            TestASTNode::Return,
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Variable("b".to_string()),
            TestASTNode::Variable("c".to_string()),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_function_call() {
        let input = "\
        func f(a: u8, b: i16, c: u64) -> u64 { return a + b + c; }
        f(1, 2, 6);
        ";
        let expected_ast = vec![
            TestASTNode::FuncDecl(vec![
                ("f".to_string(), TokenKind::U64), // function name
                ("a".to_string(), TokenKind::U8),
                ("b".to_string(), TokenKind::I16),
                ("c".to_string(), TokenKind::U64),
            ]),
            TestASTNode::Return,
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::BinaryExpr(TokenKind::Plus),
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Variable("b".to_string()),
            TestASTNode::Variable("c".to_string()),
            TestASTNode::FunctionCall("f".to_string()),
            TestASTNode::Integer(1),
            TestASTNode::Integer(2),
            TestASTNode::Integer(6),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_for_loop() {
        let input = "\
        func main() {
            var a: u32 = 0;
            for i in 0..10 {
                a += i;
            }
        }
        ";
        let expected_ast = vec![
            TestASTNode::FuncDecl(vec![("main".to_string(), TokenKind::Void)]),
            TestASTNode::Var("a".to_string(), TokenKind::U32),
            TestASTNode::Integer(0),
            TestASTNode::For("i".to_string()),
            TestASTNode::Integer(0),
            TestASTNode::Integer(10),
            TestASTNode::Assign("a".to_string()),
            TestASTNode::BinaryExpr(TokenKind::PlusEqual),
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Variable("i".to_string()),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }

    #[test]
    fn should_parse_while_loop() {
        let input = "\
        func main() {
            var a: i32 = 5;
            while a > 0 {
                a -= 1;
            }
        }
        ";
        let expected_ast = vec![
            TestASTNode::FuncDecl(vec![("main".to_string(), TokenKind::Void)]),
            TestASTNode::Var("a".to_string(), TokenKind::I32),
            TestASTNode::Integer(5),
            TestASTNode::While,
            TestASTNode::BinaryExpr(TokenKind::RightAngleBracket),
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Integer(0),
            TestASTNode::Assign("a".to_string()),
            TestASTNode::BinaryExpr(TokenKind::MinusEqual),
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Integer(1),
        ];

        let verifier = ASTVerifier::new(input, expected_ast);
        verifier.verify();
    }
}
