use crate::ast::SourceFileItem;
use crate::lexer::*;

pub fn parse(code: &str) -> Result<Vec<SourceFileItem>, String> {
    let tokens = lexer::lex(code).map_err(|e| e.to_string())?;
    let tokens = indent_process(tokens);
    let slice = SliceByRef(&tokens);

    parser::source_file(&slice).map_err(|e| e.to_string())
}

peg::parser!(pub grammar parser<'a>() for SliceByRef<'a, Token> {
    use crate::ast;
    use crate::ast::{Expression, Function, SourceFileItem, Struct};
    use crate::lexer::Token;

    pub rule source_file() -> Vec<SourceFileItem>
        = s:(source_file_item()*) { s }

    rule source_file_item() -> SourceFileItem
        = m:module() { m }
        / s:structure() { SourceFileItem::Struct(s) }
        / f:function() { SourceFileItem::Function(f) }

    rule module() -> SourceFileItem
        = [Token::NewLine]* [Token::Module] _ i:identifier() { SourceFileItem::Module(i) }

    rule function() -> Function
        = [Token::NewLine]* [Token::Function] _ name:identifier() _
        [Token::OpenParenthesis] parameters:((_ i:identifier() [Token::Colon] _ t:type_name() _ { (i, t) }) ** [Token::Comma]) [Token::CloseParenthesis] _
        ([Token::Minus] [Token::GreaterThan])? _
        return_type:type_name()? _ [Token::Colon]
        body:block() _
        { Function { name, parameters, return_type, body}}

    rule structure() -> Struct
        = [Token::NewLine]* [Token::Struct] _ name:identifier() _ [Token::Colon] _ [Token::NewLine]
        [Token::Indent] fields:((f:struct_field() { f })** [Token::NewLine])
        [Token::NewLine] [Token::Dedent]
        {
            use std::collections::BTreeMap;
            let mut map = BTreeMap::<String, ast::Type>::new();
            for field in fields {
                map.insert(field.0, field.1);
            }

            Struct { name, fields: map }
        }

    rule struct_field() -> (String, ast::Type)
        = name:identifier() [Token::Colon] _ ty:type_name() { (name, ty) }

    rule block() -> Vec<Expression>
        = [Token::NewLine] [Token::Indent] s:statements() [Token::Dedent] { s }

    rule statements() -> Vec<Expression>
        = s:(statement()*) { s }

    rule statement() -> Expression
        = _ e:expression() _ [Token::NewLine]? _ { e }

    #[cache_left_rec]
    rule expression() -> Expression
        = declare()
        / assignment()
        / if_else()
        / while_loop()
        / struct_inst()
        / returnexpr()
        / binary_op()

    rule struct_inst() -> Expression
        = [Token::NewLine]* [Token::Instantiate] _ i:identifier() _ [Token::Colon] _ [Token::NewLine]
        [Token::Indent] members:((f:struct_inst_member() { (f.0, f.1) })*) _
        [Token::Dedent]
        { Expression::StructInstantiate(i, members) }

    rule struct_inst_member() -> (String, Expression)
        = name:identifier() [Token::Colon] _ e:expression() _ [Token::NewLine]? { (name, e) }

    rule if_else() -> Expression
        = [Token::If] _ e:expression() _ [Token::Colon]
        then_body:block() _ [Token::Else] [Token::Colon]
        else_body:block()
        { Expression::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expression
        = [Token::While] _ e:expression() _ [Token::Colon]
        loop_body:block()
        { Expression::While(Box::new(e), loop_body) }

    rule declare() -> Expression
        = [Token::Let] _ [Token::Mut] _ i:identifier() [Token::Colon]? _ t:type_name()? _ [Token::Equals] _ e:expression() { Expression::Declare(i, Box::new(e), t, true) }
        / [Token::Let] _ i:identifier() [Token::Colon]? _ t:type_name()? _ [Token::Equals] _ e:expression() { Expression::Declare(i, Box::new(e), t, false) }

    rule assignment() -> Expression
        = i:identifier() _ [Token::Equals] _ e:expression() {Expression::Assign(i, Box::new(e))}

    #[cache_left_rec]
    rule binary_op() -> Expression = precedence!{
        a:@ _ [Token::Equals] [Token::Equals] _ b:(@) { Expression::Eq(Box::new(a), Box::new(b)) }
        a:@ _ [Token::ExclaimationMark] [Token::Equals] _ b:(@) { Expression::Ne(Box::new(a), Box::new(b)) }
        a:@ _ [Token::LessThan]  _ b:(@) { Expression::Lt(Box::new(a), Box::new(b)) }
        a:@ _ [Token::LessThan] [Token::Equals] _ b:(@) { Expression::Le(Box::new(a), Box::new(b)) }
        a:@ _ [Token::GreaterThan]  _ b:(@) { Expression::Gt(Box::new(a), Box::new(b)) }
        a:@ _ [Token::GreaterThan] [Token::Equals] _ b:(@) { Expression::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ _ [Token::Plus] _ b:(@) { Expression::Add(Box::new(a), Box::new(b)) }
        a:@ _ [Token::Minus] _ b:(@) { Expression::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ [Token::Asterisk] _ b:(@) { Expression::Mul(Box::new(a), Box::new(b)) }
        a:@ _ [Token::Slash] _ b:(@) { Expression::Div(Box::new(a), Box::new(b)) }
        --
        a:@ _ [Token::And] _ b:(@) { Expression::And(Box::new(a), Box::new(b)) }
        a:@ _ [Token::Or] _ b:(@) { Expression::Or(Box::new(a), Box::new(b)) }
        a:@ _ [Token::Xor] _ b:(@) { Expression::Xor(Box::new(a), Box::new(b)) }
        _ [Token::Not] _ e:expression() { Expression::Not(Box::new(e)) }
        --
        l:literal() { l }
        i:identifier() _ [Token::OpenParenthesis] args:((_ e:expression() _ {e}) ** [Token::Comma]) [Token::CloseParenthesis] { Expression::Call(i, args, None) }
        i:identifier() { Expression::Identifier(i) }
        a:@ [Token::Period] i:identifier() _ [Token::OpenParenthesis] args:((_ b:expression() _ {b}) ** [Token::Comma]) [Token::CloseParenthesis] { Expression::Call(i, args, Some(Box::new(a))) }
        --
        e:@ _ [Token::As] _ t:type_name() { Expression::Cast(Box::new(e), t) }
        e:@ [Token::Period] i:identifier() { Expression::StructMember(Box::new(e), i) }
        --
        [Token::OpenBracket] _ data:((_ b:expression() _ {b}) ** [Token::Comma]) _ [Token::CloseBracket] { Expression::Array(data) }
    }

    rule returnexpr() -> Expression
        = [Token::Return] _ e:expression()? { match e {
            Some(expr) => Expression::Return(Some(Box::new(expr))),
            None => Expression::Return(None)
        }}

    rule identifier() -> String
        = [Token::Identifier(s)] { s.clone() }

    rule literal() -> Expression = precedence!{
        [Token::Literal(l)] { Expression::Literal(l.clone()) }
        --
        [Token::Ampersand] i:identifier() { Expression::GlobalDataAddr(i) }
    }

    rule type_name() -> ast::Type
        = [Token::OpenBracket] _ t:type_name() _ [Token::CloseBracket] { ast::Type::Array(Box::new(t))}
        / [Token::Identifier(i)] {
            match i.as_str() {
                "f32" => ast::Type::F32,
                "f64" => ast::Type::F64,
                "i8" => ast::Type::I8,
                "i16" => ast::Type::I16,
                "i32" => ast::Type::I32,
                "i64" => ast::Type::I64,
                "bool" => ast::Type::Bool,
                "str" => ast::Type::String,
                _ => ast::Type::Struct(i.clone()),
            }
        }
        / expected!("type name")


    rule _() =  quiet!{[Token::Space]*}
});
