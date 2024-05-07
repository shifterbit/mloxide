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
    Bool(bool),
    Identifier(String),
    Binary {
        op: Operator,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Unary {
        op: Operator,
        expr: Box<AstNode>,
    },
    If {
        condition: Box<AstNode>,
        if_body: Box<AstNode>,
        else_body: Box<AstNode>
    }
}
