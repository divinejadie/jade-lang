use cranelift::prelude::isa::TargetIsa;
use cranelift::prelude::types;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum TypeLiteral {
    Bool(bool),
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Struct(String),
    String,
    Pointer,
    Bool,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
}

impl Type {
    pub fn to_ir(&self, isa: &dyn TargetIsa) -> types::Type {
        match self {
            Type::Struct(_) => isa.pointer_type(),
            Type::Pointer => isa.pointer_type(),
            Type::String => isa.pointer_type(),
            Type::Bool => types::B8,
            Type::F32 => types::F32,
            Type::F64 => types::F64,
            Type::I8 => types::I8,
            Type::I16 => types::I16,
            Type::I32 => types::I32,
            Type::I64 => types::I64,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            _ => false,
        }
    }
}

impl TypeLiteral {
    pub fn get_type(&self) -> Type {
        match self {
            TypeLiteral::Bool(_) => Type::Bool,
            TypeLiteral::F32(_) => Type::F32,
            TypeLiteral::F64(_) => Type::F64,
            TypeLiteral::I8(_) => Type::I8,
            TypeLiteral::I16(_) => Type::I16,
            TypeLiteral::I32(_) => Type::I32,
            TypeLiteral::I64(_) => Type::I64,
            TypeLiteral::String(_) => Type::String,
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

#[derive(Debug, Clone, Copy)]
pub enum BooleanExpr {
    And,
    Or,
    Xor,
    Not,
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
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Xor(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    IfElse(Box<Expression>, Vec<Expression>, Vec<Expression>),
    While(Box<Expression>, Vec<Expression>),
    Call(String, Vec<Expression>, Option<Box<Expression>>),
    Return(Option<Box<Expression>>),
    GlobalDataAddr(String),
    StructInstantiate(String, Vec<(String, Expression)>), // type, (member, value),
    StructMember(Box<Expression>, String),
    Cast(Box<Expression>, Type),
}

#[derive(Debug, Clone)]
pub enum SourceFileItem {
    Module(String),
    Function(Function),
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: BTreeMap<String, Type>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Expression>,
}
