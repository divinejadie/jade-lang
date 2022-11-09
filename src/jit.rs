use super::ast;
use super::ast::{Expression, TypeLiteral};
use super::grammar;
use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

pub struct Jit {
    builder_context: FunctionBuilderContext,
    context: codegen::Context,
    data_context: DataContext,
    module: JITModule,
}

impl Default for Jit {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());
        Self {
            builder_context: FunctionBuilderContext::new(),
            context: module.make_context(),
            data_context: DataContext::new(),
            module,
        }
    }
}

impl Jit {
    pub fn compile(&mut self, input: &str) -> Result<*const u8, String> {
        let (name, params, the_return, statements) =
            grammar::parser::function(input).map_err(|e| e.to_string())?;

        self.translate(params, statements)
    }
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {}
    fn translate(
        &mut self,
        params: Vec<(String, types::Type)>,
        the_return: (String, types::Type),
        statements: Vec<Expression>,
    ) -> Result<(), String> {
        for (_, ty) in &params {
            self.context.func.signature.params.push(AbiParam::new(*ty));
        }

        self.context
            .func
            .signature
            .returns
            .push(AbiParam::new(the_return.1));

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let variables =
            declare_variables(&mut builder, &params, &the_return, &statements, entry_block);

        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
        };

        for expr in statements {
            trans.translate_expression(expr);
        }

        let return_variable = trans.variables.get(&the_return).unwrap();
        let return_value = trans.builder.use_var(*return_variable);

        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();

        Ok(())
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, (types::Type, Variable)>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_expression(&mut self, expression: Expression) -> Value {
        match expression {
            Expression::Literal(literal) => match literal {
                TypeLiteral::F32(val) => self.builder.ins().f32const(Ieee32::with_float(val)),
                TypeLiteral::I32(val) => self.builder.ins().iconst(types::I32, val),
                _ => (),
            },
            Expression::Add(lhs, rhs) => {
                let lhs = self.translate_expression(*lhs);
                let rhs = self.translate_expression(*rhs);
                self.builder.ins()
            }
            _ => (),
        }
    }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    params: &[(String, types::Type)],
    return_: &(String, types::Type),
    statements: &[Expression],
    entry_block: Block,
) -> HashMap<String, (Variable, types::Type)> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, (name, ty)) in params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(*ty, val, builder, &mut variables, &mut index, name);
    }

    let zero = builder.ins().iconst(types::I32, 0);
    let return_variable = declare_variable(
        return_.1,
        zero,
        builder,
        &mut variables,
        &mut index,
        &return_.0,
    );

    for expr in statements {
        declare_variables_in_statement(builder, &mut variables, &mut index, expr);
    }

    variables
}

fn declare_variables_in_statement(
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, (Variable, types::Type)>,
    index: &mut usize,
    expression: &Expression,
) {
    let zero = builder.ins().iconst(types::I32, 0);
    match *expression {
        Expression::Assign(ref name, _, ref ty) => {
            // TODO: Infer type
            declare_variable(
                ty.expect("Type inference not implemented"),
                zero,
                builder,
                variables,
                index,
                name,
            );
        }
        Expression::IfElse(ref _cond, ref then_body, ref else_body) => {
            for statement in then_body {
                declare_variables_in_statement(builder, variables, index, statement)
            }
            for statement in else_body {
                declare_variables_in_statement(builder, variables, index, statement)
            }
        }
        Expression::While(ref _cond, ref loop_body) => {
            for statement in loop_body {
                declare_variables_in_statement(builder, variables, index, statement)
            }
        }
        _ => (),
    }
}

fn declare_variable(
    ty: types::Type,
    value: Value,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, (Variable, types::Type)>,
    index: &mut usize,
    name: &str,
) -> Variable {
    let var = Variable::new(*index);
    variables.insert(name.into(), (var, ty));
    builder.declare_var(var, ty);
    *index += 1;

    // Variables always have a value
    builder.def_var(var, value);

    var
}

fn find_expression_type(
    expression: &Expression,
    variables: &mut HashMap<String, (Variable, types::Type)>,
) -> Option<types::Type> {
    // All literals are of same type
    let binary = |rhs: &Box<Expression>, lhs: &Box<Expression>| -> Option<types::Type> {
        let left = find_expression_type(lhs, variables);
        let right = find_expression_type(rhs, variables);
        if let Some(l) = left
            && let Some(r) = right
        {
            if l == r {
                left
            }
        } else {
            None
        }
    };
    match expression {
        Expression::Literal(literal) => Some(literal.get_type()),
        Expression::Identifier(ident) => Some(variables.get(ident).unwrap().1),
        Expression::Eq(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Lt(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Le(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Gt(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Ge(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Add(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Sub(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Mul(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Div(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Assign(_, _, _) => None,

        _ => None,
    }
}

