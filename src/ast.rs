use cranelift::prelude::*;

#[derive(Debug, Clone)]
pub enum TypeLiteral {
    Bool(bool),
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl TypeLiteral {
    pub fn get_type(&self) -> types::Type {
        match self {
            TypeLiteral::Bool(_) => types::B8,
            TypeLiteral::F32(_) => types::F32,
            TypeLiteral::F64(_) => types::F64,
            TypeLiteral::I8(_) => types::I8,
            TypeLiteral::I16(_) => types::I16,
            TypeLiteral::I32(_) => types::I32,
            TypeLiteral::I64(_) => types::I64,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(TypeLiteral),
    Identifier(String),
    Declare(String, Box<Expression>, Option<Type>, bool),
    Assign(String, Box<Expression>),
    Eq(Box<Expression>, Box<Expression>),
    Ne(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Le(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Ge(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    IfElse(Box<Expression>, Vec<Expression>, Vec<Expression>),
    While(Box<Expression>, Vec<Expression>),
    Call(String, Vec<Expression>, Option<Box<Expression>>),
    Return(Option<Box<Expression>>),
    GlobalDataAddr(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Expression>,
}

