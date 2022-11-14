use cranelift::codegen::ir::types::*;
use cranelift::prelude::*;

peg::parser!(pub grammar mod_scan() for str {
    pub rule modules() -> Vec<String>
        = [' ' | '\t' | '\n']* m:(p_mod()** "\n") _ quiet!{[_]*} { m }

    rule p_mod() -> String
        = "module" _ i:identifier() _ { i }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule _() =  quiet!{[' ' | '\t']*}
});

peg::parser!(pub grammar parser() for str {
    use crate::ast::{Expression, Function, TypeLiteral, SourceFileItem, Struct, StructField};

    pub rule source_file() -> Vec<SourceFileItem>
        = s:(source_file_item()** "\n") { s }

    rule source_file_item() -> SourceFileItem
        = m:module() { m }
        / s:structure() { SourceFileItem::Struct(s) }
        / f:function() { SourceFileItem::Function(f) }

    pub rule file() -> Vec<Function>
        = f:(function()** "\n") { f }

    rule module() -> SourceFileItem
        = [' ' | '\t' | '\n']* "module" _ i:identifier() { SourceFileItem::Module(i) }

    rule function() -> Function
        = [' ' | '\t' | '\n']* "fn" _ name:identifier() _
        "(" parameters:((_ i:identifier() ":" _ t:type_name() _ { (i, t) }) ** ",") ")" _
        "->"? _?
        return_type:type_name()? _?
        "{" _ "\n"?
        body:statements()
        _ "}" "\n" _
        { Function { name, parameters, return_type, body}}

    rule structure() -> Struct
        = [' ' | '\t' | '\n']* "struct" _ name:identifier() _
        "{" _ "\n"? _
        fields:((f:struct_field() { f })** "\n")
        "}" _ "\n"?
        { Struct { name, fields } }

    rule struct_field() -> StructField
        = name:identifier() ":" _ ty:type_name() { StructField { name, ty } }

    rule statements() -> Vec<Expression>
        = s:(statement()*) { s }

    rule statement() -> Expression
        = _ e:expression() _ "\n" { e }

    #[cache_left_rec]
    rule expression() -> Expression
        = if_else()
        / while_loop()
        / declare()
        / assignment()
        / returnexpr()
        / binary_op()

    rule if_else() -> Expression
        = "if" _ e:expression() _ "{" "\n"?
        then_body:statements() _ "}" _ "else" _ "{" _ "\n"
        else_body:statements()
        "\n"? _ "}" _
        { Expression::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Expression
        = "while" _ e:expression() _ "{" "\n"?
        loop_body:statements() _ "\n"? _ "}" _
        { Expression::While(Box::new(e), loop_body) }

    rule declare() -> Expression
        = "let" _ "mut" _ i:identifier() ":"? _ t:type_name()? _ "=" _ e:expression() { Expression::Declare(i, Box::new(e), t, true) }
        / "let" _ i:identifier() ":"? _ t:type_name()? _ "=" _ e:expression() { Expression::Declare(i, Box::new(e), t, false) }

    rule assignment() -> Expression
        = i:identifier() _ "=" _ e:expression() {Expression::Assign(i, Box::new(e))}

    #[cache_left_rec]
    rule binary_op() -> Expression = precedence!{
        a:@ _ "==" _ b:(@) { Expression::Eq(Box::new(a), Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Expression::Ne(Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Expression::Lt(Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Expression::Le(Box::new(a), Box::new(b)) }
        a:@ _ ">"  _ b:(@) { Expression::Gt(Box::new(a), Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Expression::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Expression::Add(Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Expression::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Expression::Mul(Box::new(a), Box::new(b)) }
        a:@ _ "/" _ b:(@) { Expression::Div(Box::new(a), Box::new(b)) }
        --
        a:@ _ "and" _ b:(@) { Expression::And(Box::new(a), Box::new(b)) }
        a:@ _ "or" _ b:(@) { Expression::Or(Box::new(a), Box::new(b)) }
        a:@ _ "xor" _ b:(@) { Expression::Xor(Box::new(a), Box::new(b)) }
        _ "not" _ e:expression() { Expression::Not(Box::new(e)) }
        --
        a:@ "." i:identifier() _ "(" args:((_ b:expression() _ {b}) ** ",") ")" { Expression::Call(i, args, Some(Box::new(a))) }
        --
        l:literal() { l }
        i:identifier() _ "(" args:((_ e:expression() _ {e}) ** ",") ")" { Expression::Call(i, args, None) }
        i:identifier() { Expression::Identifier(i) }
    }

    rule returnexpr() -> Expression
        = "return" _ e:expression()? { match e {
            Some(expr) => Expression::Return(Some(Box::new(expr))),
            None => Expression::Return(None)
        }}

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule type_literal() -> TypeLiteral = precedence!{
        d:$(['0'..='9']+) "." e:$(['0'..='9']+) { TypeLiteral::F32(format!("{}.{}", d, e).parse::<f32>().unwrap()) }
        n:$(['0'..='9']+) { TypeLiteral::I32(n.parse::<i32>().unwrap()) }
        "true" { TypeLiteral::Bool(true) }
        "false" { TypeLiteral::Bool(false) }
    }

    rule type_name() -> Type
        = "f32" { types::F32 }
        / "f64" { types::F64 }
        / "i8" { types::I8 }
        / "i16" { types::I16 }
        / "i32" { types::I32 }
        / "i64" { types::I64 }
        / "bool" { types::B8 }
        / expected!("type name")

    rule literal() -> Expression = precedence!{
        t:type_literal() { Expression::Literal(t) }
        --
        "&" i:identifier() { Expression::GlobalDataAddr(i) }
    }

    rule _() =  quiet!{[' ' | '\t']*}
});

