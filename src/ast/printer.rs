use super::ASTVisitor;

use termion::color::Fg;
use termion::color::{self, White};

pub struct ASTTreePrinter {
    indentation: usize,
}

impl ASTTreePrinter {
    const INDENATION: usize = 2;

    const TEXT_COLOR: color::White = color::White;
    const STATEMENT_COLOR: color::Yellow = color::Yellow;
    const LET_STATEMENT_COLOR: color::Green = color::Green;
    const EXPR_COLOR: color::Green = color::Green;
    const BIN_EXPR_COLOR: color::LightBlue = color::LightBlue;
    const OPERATOR_COLOR: color::LightYellow = color::LightYellow;

    const STATEMENT_ICON: &str = nerd_font_symbols::md::MD_SIGMA;
    const LET_STATEMENT_ICON: &str = nerd_font_symbols::md::MD_EQUAL;
    const FUNC_STATEMENT_ICON: &str = nerd_font_symbols::md::MD_FUNCTION_VARIANT;
    const FUNC_CALL_STATEMENT_ICON: &str = nerd_font_symbols::md::MD_FUNCTION;
    const EXPR_ICON: &str = nerd_font_symbols::md::MD_FUNCTION_VARIANT;
    const BIN_EXPR_ICON: &str = nerd_font_symbols::cod::COD_SYMBOL_OPERATOR;
    const VARIABLE_ICON: &str = nerd_font_symbols::md::MD_VARIABLE;

    pub fn new() -> Self {
        Self { indentation: 0 }
    }

    fn increase_indentation(&mut self) {
        self.indentation += Self::INDENATION;
    }
    fn decrease_indentation(&mut self) {
        self.indentation -= Self::INDENATION;
    }

    fn print(&self, text: &str, text_color: &dyn color::Color) {
        // println!("{}├─ {}", "│ ".repeat(self.indentation), text);
        println!(
            "│{}└─ {}{}{}",
            " ".repeat(self.indentation),
            color::Fg(text_color),
            text,
            color::Fg(color::Reset)
        );
    }
}

impl ASTVisitor<()> for ASTTreePrinter {
    fn visit_statement(&mut self, statement: &super::ASTStatement) {
        self.print(
            &format!("{}  Statement:", Self::STATEMENT_ICON),
            &Self::STATEMENT_COLOR,
        );
        self.increase_indentation();
        ASTVisitor::do_visit_statement(self, statement);
        self.decrease_indentation();
    }

    fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) -> () {
        for statement in statement.statements.iter() {
            self.visit_statement(statement);
        }
    }

    fn visit_return_statement(&mut self, statement: &super::ASTReturnStatement) {
        self.print(
            &format!("{}  Return:", Self::LET_STATEMENT_ICON),
            &Self::LET_STATEMENT_COLOR,
        );
        self.increase_indentation();
        ASTVisitor::do_visit_expression(self, &statement.expr);
        self.decrease_indentation();
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) {
        self.print(
            &format!(
                "{}  Declaration(Let): {}{}",
                Self::LET_STATEMENT_ICON,
                color::Fg(Self::TEXT_COLOR),
                &statement.identifier.span.literal
            ),
            &Self::LET_STATEMENT_COLOR,
        );
        self.increase_indentation();
        self.print(
            &format!(
                "DataType: {}{}",
                Fg(Self::TEXT_COLOR),
                statement.data_type.span.literal
            ),
            &Self::TEXT_COLOR,
        );
        ASTVisitor::do_visit_expression(self, &statement.initializer);
        self.decrease_indentation();
    }

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) {
        self.print(
            &format!(
                "{}  Declaration(Var): {}{}",
                Self::LET_STATEMENT_ICON,
                color::Fg(Self::TEXT_COLOR),
                &statement.identifier.span.literal
            ),
            &Self::LET_STATEMENT_COLOR,
        );
        self.increase_indentation();
        self.print(
            &format!(
                "DataType: {}{}",
                Fg(Self::TEXT_COLOR),
                statement.data_type.span.literal
            ),
            &Self::TEXT_COLOR,
        );
        ASTVisitor::do_visit_expression(self, &statement.initializer);
        self.decrease_indentation();
    }

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) {
        self.print("If:", &color::Blue);
        self.increase_indentation();
        self.visit_expression(&statement.condition);
        self.print("Then:", &Self::TEXT_COLOR);
        self.increase_indentation();
        self.visit_statement(&statement.then_branch);
        self.decrease_indentation();
        if let Some(else_branch) = &statement.else_branch {
            self.print("Else:", &Self::TEXT_COLOR);
            self.increase_indentation();
            self.visit_statement(&else_branch.else_branch);
        }
        self.decrease_indentation();
    }

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) {
        self.print(
            &format!("For: {}", statement.loop_variable.span.literal),
            &color::Blue,
        );
        self.increase_indentation();
        self.print("From:", &Self::TEXT_COLOR);
        self.increase_indentation();
        self.visit_expression(&statement.range.0);
        self.decrease_indentation();
        self.print("To:", &Self::TEXT_COLOR);
        self.increase_indentation();
        self.visit_expression(&statement.range.1);
        self.decrease_indentation();
        self.print("Body:", &Self::TEXT_COLOR);
        self.increase_indentation();
        self.visit_statement(&statement.body);
        self.decrease_indentation();
    }
    fn visit_while_loop_statement(&mut self, statement: &super::ASTWhileStatement) {
        self.print("While:", &color::Blue);
        self.increase_indentation();
        self.print("Condition:", &Self::TEXT_COLOR);
        self.increase_indentation();
        self.visit_expression(&statement.condition);
        self.decrease_indentation();
        self.print("Body:", &Self::TEXT_COLOR);
        self.increase_indentation();
        self.visit_statement(&statement.body);
        self.decrease_indentation();
    }

    fn visit_function_statement(&mut self, function: &super::ASTFunctionStatement) {
        self.print(
            &format!(
                "{}  Function: {}{}",
                Self::FUNC_STATEMENT_ICON,
                color::Fg(Self::TEXT_COLOR),
                &function.identifier.span.literal
            ),
            &Self::TEXT_COLOR,
        );

        self.increase_indentation();
        self.print(&format!("Arguments:"), &Self::TEXT_COLOR);
        self.increase_indentation();
        for arg in function.arguments.iter() {
            self.print(
                &format!(
                    "{}  Argument: {}{} ({})",
                    Self::FUNC_STATEMENT_ICON,
                    color::Fg(Self::TEXT_COLOR),
                    &arg.identifier.span.literal,
                    &arg.data_type.span.literal
                ),
                &Self::TEXT_COLOR,
            );
        }
        self.decrease_indentation();

        self.print(&format!("Body:"), &Self::TEXT_COLOR);
        self.increase_indentation();

        if let super::ASTStatementKind::Compound(statement) = &function.body.kind {
            self.visit_compound_statement(statement);
        }
        self.decrease_indentation();
    }

    fn visit_struct_statement(&mut self, struct_def: &super::ASTStructStatement) {
        self.print(
            &format!(
                "{}  Struct: {}{}",
                Self::FUNC_STATEMENT_ICON,
                color::Fg(Self::TEXT_COLOR),
                &struct_def.identifier.span.literal
            ),
            &Self::TEXT_COLOR,
        );

        self.increase_indentation();
        for member in struct_def.members.iter() {
            self.print(
                &format!(
                    "{}  Member: {}{} ({})",
                    Self::FUNC_STATEMENT_ICON,
                    color::Fg(Self::TEXT_COLOR),
                    &member.identifier.span.literal,
                    &member.data_type.span.literal
                ),
                &Self::TEXT_COLOR,
            );
        }
        self.decrease_indentation();
    }

    fn visit_expression(&mut self, expr: &super::ASTExpression) {
        // self.print(
        //     &format!("{}  Expression:", Self::EXPR_ICON),
        //     &Self::EXPR_COLOR,
        // );
        // self.increase_indentation();
        ASTVisitor::do_visit_expression(self, &expr);
        // self.decrease_indentation();
    }

    fn visit_assignment_expression(&mut self, expr: &super::ASTAssignmentExpression) {
        self.print("Assignment:", &color::Blue);
        self.print(
            &format!(
                "{}  Assignment: {}{}",
                nerd_font_symbols::md::MD_EQUAL,
                color::Fg(Self::OPERATOR_COLOR),
                expr.identifier.span.literal
            ),
            &Self::TEXT_COLOR,
        );
        self.increase_indentation();
        self.visit_expression(&expr.expr);
    }

    fn visit_function_call_expression(&mut self, expr: &super::ASTFunctionCallExpression) {
        self.print(
            &format!(
                "{}  FunctionCall: {}{}",
                Self::FUNC_CALL_STATEMENT_ICON,
                color::Fg(Self::TEXT_COLOR),
                &expr.identifier.span.literal
            ),
            &Self::TEXT_COLOR,
        );
        self.increase_indentation();
        for expr in expr.arguments.iter() {
            ASTVisitor::do_visit_expression(self, &expr);
        }
        self.decrease_indentation();
    }

    fn visit_variable_expression(&mut self, expr: &super::ASTVariableExpression) {
        self.print(
            &format!("{}  Variable: {}", Self::VARIABLE_ICON, expr.identifier()),
            &Self::TEXT_COLOR,
        );
    }

    fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) {
        self.print(
            &format!(
                "{}  Unary: {}{}",
                Self::BIN_EXPR_ICON,
                color::Fg(Self::OPERATOR_COLOR),
                expr.operator.token.span.literal
            ),
            &Self::BIN_EXPR_COLOR,
        );
        self.increase_indentation();
        self.visit_expression(&expr.expr);
        self.decrease_indentation();
    }

    fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) {
        self.print(
            &format!(
                "{}  Binary: {}{}",
                Self::BIN_EXPR_ICON,
                color::Fg(Self::OPERATOR_COLOR),
                expr.operator.token.span.literal
            ),
            &Self::BIN_EXPR_COLOR,
        );
        self.increase_indentation();
        // self.print_binary_operator(&expr.operator);
        // self.print(&format!("{:?}", expr.operator.kind), &Self::TEXT_COLOR);
        self.visit_expression(&expr.left);
        self.visit_expression(&expr.right);
        self.decrease_indentation();
    }

    fn visit_parenthesised_expression(&mut self, expr: &super::ASTParenthesizedExpression) {
        self.print(
            &format!(
                "{}  Parenthesized:",
                nerd_font_symbols::md::MD_CODE_PARENTHESES
            ),
            &color::Magenta,
        );
        self.increase_indentation();
        self.visit_expression(&expr.expr);
    }

    fn visit_binary_operator(&mut self, op: &super::ASTBinaryOperator) {
        let var_name = format!(
            "Operator: {}",
            match op.kind {
                super::ASTBinaryOperatorKind::Plus => "+",
                super::ASTBinaryOperatorKind::Minus => "-",
                super::ASTBinaryOperatorKind::Multiply => "*",
                super::ASTBinaryOperatorKind::Divide => "/",
                super::ASTBinaryOperatorKind::EqualTo => "==",
                super::ASTBinaryOperatorKind::NotEqualTo => "!=",
                super::ASTBinaryOperatorKind::LogicAND => "&&",
                super::ASTBinaryOperatorKind::LogicOR => "||",
                super::ASTBinaryOperatorKind::GreaterThan => ">",
                super::ASTBinaryOperatorKind::GreaterThanOrEqual => ">=",
                super::ASTBinaryOperatorKind::LessThan => "<",
                super::ASTBinaryOperatorKind::LessThanOrEqual => "<=",
                super::ASTBinaryOperatorKind::BitwiseOR => "|",
                super::ASTBinaryOperatorKind::BitwiseAND => "&",
                super::ASTBinaryOperatorKind::BitwiseXOR => "^",
            }
        );
        self.print(&var_name, &color::Yellow);
    }

    fn visit_error(&mut self, span: &super::TextSpan) {
        self.print(&format!("Error: {:?}", span), &color::Red);
    }

    fn visit_integer(&mut self, integer: &i64) {
        self.print(&format!("Integer: {}", integer), &Self::TEXT_COLOR);
    }

    fn visit_boolean(&mut self, boolean: bool) {
        self.print(&format!("Integer: {}", boolean), &Self::TEXT_COLOR);
    }

    fn visit_float(&mut self, float: &f64) {
        self.print(&format!("Float: {}", float), &Self::TEXT_COLOR);
    }
}

pub struct ASTHiglightPrinter {
    indent: usize,
    result: String,
}

impl ASTHiglightPrinter {
    const INDENATION: usize = 2;

    const KEYWORD_COLOR: color::Green = color::Green;
    const TYPE_COLOR: color::Cyan = color::Cyan;
    const OPERATOR_COLOR: color::Green = color::Green;
    const TEXT_COLOR: color::White = color::White;

    const INTEGER_COLOR: color::Cyan = color::Cyan;
    const FLOAT_COLOR: color::Cyan = color::Cyan;
    const LET_COLOR: color::Green = color::Green;
    const FUNC_COLOR: color::Green = color::Green;
    const FUNC_CALL_COLOR: color::Yellow = color::Yellow;
    const FUNC_NAME_COLOR: color::Yellow = color::Yellow;
    const VARIABLE_COLOR: color::LightWhite = color::LightWhite;

    pub fn new() -> Self {
        Self {
            indent: 0,
            result: "".to_string(),
        }
    }

    pub fn print_result(&self) {
        let decoration = "=".repeat(80) + "\n";
        println!(
            "{decoration}Highlighted Source:\n{decoration}{}\n{}{decoration}",
            self.result
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{:3} │ {}", i + 1, line))
                .collect::<Vec<String>>()
                .join("\n"),
            Fg(White)
        );
    }

    fn add_whitespace(&mut self) {
        self.print(" ");
    }

    fn add_newline(&mut self) {
        self.print("\n");
    }

    fn add_semicolon(&mut self) {
        self.print(";");
    }

    fn increase_indentation(&mut self) {
        self.indent += Self::INDENATION;
    }
    fn decrease_indentation(&mut self) {
        self.indent -= Self::INDENATION;
    }

    fn print_indent(&mut self) {
        self.result.push_str(&format!(
            "{}{}",
            " ".repeat(self.indent),
            color::Fg(color::Reset)
        ));
    }
    fn print_with_indent(&mut self, text: &str) {
        self.result.push_str(&format!(
            "{}{}{}",
            " ".repeat(self.indent),
            text,
            color::Fg(color::Reset)
        ));
    }
    fn print(&mut self, text: &str) {
        self.result
            .push_str(&format!("{}{}", text, color::Fg(color::Reset)));
    }

    fn visit_idenifier(&mut self, identifier: &String) {
        self.print(&format!("{}{}", Fg(Self::TEXT_COLOR), identifier));
    }
}

impl ASTVisitor<()> for ASTHiglightPrinter {
    fn visit_statement(&mut self, statement: &super::ASTStatement) {
        self.do_visit_statement(statement);
    }

    fn visit_return_statement(&mut self, statement: &super::ASTReturnStatement) {
        self.print_with_indent(&format!("{}return", Fg(Self::LET_COLOR)));
        self.add_whitespace();
        self.visit_expression(&statement.expr);
        self.add_semicolon();
        self.add_newline();
    }

    fn visit_let_statement(&mut self, statement: &super::ASTLetStatement) {
        self.print_with_indent(&format!("{}let", Fg(Self::LET_COLOR)));
        self.add_whitespace();
        self.visit_idenifier(&statement.identifier.span.literal);
        self.print(&format!(
            ": {}{}",
            Fg(Self::TYPE_COLOR),
            statement.data_type.span.literal
        ));
        self.add_whitespace();
        self.print(&format!("{}=", Fg(Self::TEXT_COLOR)));
        self.add_whitespace();
        self.visit_expression(&statement.initializer);
        self.add_semicolon();
        self.add_newline();
    }

    fn visit_var_statement(&mut self, statement: &super::ASTVarStatement) {
        self.print_with_indent(&format!("{}var", Fg(Self::LET_COLOR)));
        self.add_whitespace();
        self.visit_idenifier(&statement.identifier.span.literal);
        self.print(&format!(
            ": {}{}",
            Fg(Self::TYPE_COLOR),
            statement.data_type.span.literal
        ));
        self.add_whitespace();
        self.print(&format!("{}=", Fg(Self::TEXT_COLOR)));
        self.add_whitespace();
        self.visit_expression(&statement.initializer);
        self.add_semicolon();
        self.add_newline();
    }

    fn visit_compound_statement(&mut self, statement: &super::ASTCompoundStatement) {
        self.print(&format!("{}{}", Fg(Self::TEXT_COLOR), '{'));
        self.add_newline();
        self.increase_indentation();
        for statement in statement.statements.iter() {
            self.visit_statement(statement);
        }
        self.decrease_indentation();
        self.print_with_indent(&format!("{}{}", Fg(Self::TEXT_COLOR), '}'));
    }

    fn visit_if_statement(&mut self, statement: &super::ASTIfStatement) {
        self.print_with_indent(&format!(
            "{}if{} ",
            Fg(Self::KEYWORD_COLOR),
            Fg(Self::TEXT_COLOR),
        ));
        self.visit_expression(&statement.condition);
        self.add_whitespace();
        // self.increase_indentation();
        // self.increase_indentation();
        self.visit_statement(&statement.then_branch);
        if let Some(else_branch) = &statement.else_branch {
            self.print(&format!(
                "{} else{} ",
                Fg(Self::KEYWORD_COLOR),
                Fg(Self::TEXT_COLOR),
            ));
            self.visit_statement(&else_branch.else_branch);
        }
        self.add_newline();
    }

    fn visit_for_loop_statement(&mut self, statement: &super::ASTForStatement) {
        self.print_with_indent(&format!(
            "{}for{} {} in",
            Fg(Self::KEYWORD_COLOR),
            Fg(Self::TEXT_COLOR),
            statement.loop_variable.span.literal
        ));
        self.add_whitespace();
        self.visit_expression(&statement.range.0);
        self.print("..");
        self.visit_expression(&statement.range.1);
        self.add_whitespace();
        self.visit_statement(&statement.body);
        self.add_newline();
    }

    fn visit_while_loop_statement(&mut self, statement: &super::ASTWhileStatement) {
        self.print_with_indent(&format!(
            "{}while{}",
            Fg(Self::KEYWORD_COLOR),
            Fg(Self::TEXT_COLOR),
        ));
        self.add_whitespace();
        self.visit_expression(&statement.condition);
        self.add_whitespace();
        self.visit_statement(&statement.body);
        self.add_newline();
    }

    fn visit_function_statement(&mut self, function: &super::ASTFunctionStatement) {
        self.print_with_indent(&format!(
            "{}func {}{}{}(",
            Fg(Self::FUNC_COLOR),
            Fg(Self::FUNC_NAME_COLOR),
            function.identifier.span.literal,
            Fg(Self::TEXT_COLOR),
        ));
        for (i, arg) in function.arguments.iter().enumerate() {
            if i != 0 {
                self.print(&format!("{},", Fg(Self::TEXT_COLOR)));
                self.add_whitespace();
            }
            self.print(&format!(
                "{}{}: {}{}",
                Fg(Self::TEXT_COLOR),
                arg.identifier.span.literal,
                Fg(Self::TYPE_COLOR),
                arg.data_type.span.literal,
            ));
        }

        self.print(&format!(
            "{}) -> {} ",
            Fg(Self::TEXT_COLOR),
            function.return_type.name()
        ));
        if let super::ASTStatementKind::Compound(statement) = &function.body.kind {
            self.visit_compound_statement(statement);
        }
        self.add_newline();
    }

    fn visit_struct_statement(&mut self, struct_def: &super::ASTStructStatement) {
        self.print_with_indent(&format!(
            "{}struct {}{}{} {}",
            Fg(Self::FUNC_COLOR),
            Fg(Self::FUNC_NAME_COLOR),
            struct_def.identifier.span.literal,
            Fg(Self::TEXT_COLOR),
            '{'
        ));
        self.add_newline();
        self.increase_indentation();
        for member in struct_def.members.iter() {
            self.print(&format!(
                "{}{}: {}{},",
                Fg(Self::TEXT_COLOR),
                member.identifier.span.literal,
                Fg(Self::TYPE_COLOR),
                member.data_type.span.literal,
            ));
            self.add_newline();
        }

        self.decrease_indentation();
        self.print(&format!("{};", '}'));
        self.add_newline();
    }

    fn visit_assignment_expression(&mut self, expr: &super::ASTAssignmentExpression) {
        self.print_with_indent(&format!(
            "{}{}{} = ",
            Fg(Self::VARIABLE_COLOR),
            expr.identifier.span.literal,
            Fg(Self::TEXT_COLOR)
        ));
        self.visit_expression(&expr.expr);
        self.add_semicolon();
        self.add_newline();
    }

    fn visit_function_call_expression(&mut self, expr: &super::ASTFunctionCallExpression) {
        if expr.identifier() == "println" {
            println!("Println call with stuff...");
            return;
        }
        self.print(&format!(
            "{}{}{}(",
            Fg(Self::FUNC_CALL_COLOR),
            expr.identifier(),
            Fg(Self::TEXT_COLOR)
        ));

        for (i, arg) in expr.arguments.iter().enumerate() {
            if i != 0 {
                self.print(&format!("{},", Fg(Self::TEXT_COLOR)));
                self.add_whitespace();
            }
            self.visit_expression(arg);
        }
        self.print(&format!("{})", Fg(Self::TEXT_COLOR)));
    }

    fn visit_variable_expression(&mut self, expr: &super::ASTVariableExpression) {
        self.print(&format!(
            "{}{}",
            Fg(Self::VARIABLE_COLOR),
            expr.identifier()
        ));
    }

    fn visit_unary_expression(&mut self, expr: &super::ASTUnaryExpression) {
        self.print(&format!(
            "{}{}",
            Fg(Self::TEXT_COLOR),
            expr.operator.token.span.literal
        ));
        self.visit_expression(&expr.expr);
    }

    fn visit_binary_expression(&mut self, expr: &super::ASTBinaryExpression) {
        self.visit_expression(&expr.left);
        self.add_whitespace();
        self.print(&format!(
            "{}{}",
            Fg(Self::TEXT_COLOR),
            expr.operator.token.span.literal
        ));
        self.add_whitespace();
        self.visit_expression(&expr.right);
    }

    fn visit_parenthesised_expression(&mut self, expr: &super::ASTParenthesizedExpression) {
        self.print(&format!("{}(", Fg(Self::TEXT_COLOR)));
        self.visit_expression(&expr.expr);
        self.print(&format!("{})", Fg(Self::TEXT_COLOR)));
    }

    fn visit_binary_operator(&mut self, op: &super::ASTBinaryOperator) {
        self.print(&format!(
            "{}{}",
            Fg(Self::TEXT_COLOR),
            match op.kind {
                super::ASTBinaryOperatorKind::Plus => "+",
                super::ASTBinaryOperatorKind::Minus => "-",
                super::ASTBinaryOperatorKind::Multiply => "*",
                super::ASTBinaryOperatorKind::Divide => "/",
                super::ASTBinaryOperatorKind::EqualTo => "==",
                super::ASTBinaryOperatorKind::NotEqualTo => "!=",
                super::ASTBinaryOperatorKind::LogicAND => "&&",
                super::ASTBinaryOperatorKind::LogicOR => "||",
                super::ASTBinaryOperatorKind::GreaterThan => ">",
                super::ASTBinaryOperatorKind::GreaterThanOrEqual => ">=",
                super::ASTBinaryOperatorKind::LessThan => "<",
                super::ASTBinaryOperatorKind::LessThanOrEqual => "<=",
                super::ASTBinaryOperatorKind::BitwiseOR => "|",
                super::ASTBinaryOperatorKind::BitwiseAND => "&",
                super::ASTBinaryOperatorKind::BitwiseXOR => "^",
            }
        ));
    }

    fn visit_error(&mut self, span: &super::lexer::TextSpan) -> () {}
    fn visit_integer(&mut self, integer: &i64) {
        self.print(&format!("{}{}", Fg(Self::INTEGER_COLOR), integer));
    }
    fn visit_boolean(&mut self, boolean: bool) {
        self.print(&format!("{}{}", Fg(Self::INTEGER_COLOR), boolean));
    }

    fn visit_float(&mut self, float: &f64) {
        self.print(&format!("{}{}f", Fg(Self::FLOAT_COLOR), float));
    }
}
