use std::fmt::{self, Display};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    Declarations(Vec<Type>),
    Tuple(Vec<Type>),
    Unit,
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "()"),
            Type::Unknown => write!(f, "Unknown"),
            Type::Declarations(declarations) => {
                let mut msg = "[".to_string();
                for decl in declarations {
                    msg.push_str(&(" ".to_string() + &decl.to_string()));
                }
                msg.push_str(" ]");
                write!(f, "{}", msg)
            }
            Type::Tuple(types) => {
                let mut msg = String::new();
                for item in types {
                    msg.push_str(&(" * ".to_string() + &item.to_string()));
                }
                msg = msg.trim().strip_prefix('*').unwrap().to_owned();
                let display = msg.split_at(1).1;

                write!(f, "({})", display)
            }
        }
    }
}