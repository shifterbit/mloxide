#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Negate,
    Divide,
    Multiply,
    Equal,
    NotEqual
}
#[derive(Debug)]
pub enum AstNode {
    Int(i64),
    Float(f64),
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
