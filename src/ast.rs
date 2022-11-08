#[derive(Debug)]
pub enum TypeLiteral {
    F32(f32),
    F64(f64),
    I32(i32),
    I64(i64),
    Str(String),
}

#[derive(Debug)]
pub enum Type {
    Float32,
    Float64,
    Int32,
    Int64,
    Str,
}

#[derive(Debug)]
pub enum Expression {
    Literal(TypeLiteral),
    Identifier(String),
    Assign(String, Box<Expression>, Option<Type>),
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
    Call(String, Vec<Expression>),
    Return(Option<Box<Expression>>),
    GlobalDataAddr(String),
}

