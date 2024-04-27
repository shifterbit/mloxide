#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Negation,
    Divide,
    Multiply,
    Equal,
    NotEqual
}
#[derive(Debug)]
pub enum AstNode {
    Int(i64),
    Real(f64),
    Binary {
        op: Operator,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Unary {
        op: Operator,
        expr: Box<AstNode>,
    },
}
