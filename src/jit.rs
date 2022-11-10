use super::ast;
use super::ast::{Comparison, Expression, TypeLiteral};
use super::grammar;

use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use either::*;

pub struct Jit {
    builder_context: FunctionBuilderContext,
    context: codegen::Context,
    data_context: DataContext,
    module: JITModule,
    functions: HashMap<String, ast::Function>,
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
            functions: HashMap::new(),
        }
    }
}

impl Jit {
    pub fn compile(&mut self, input: &str) -> Result<*const u8, String> {
        let functions = grammar::parser::file(input).map_err(|e| e.to_string())?;
        let mut map: HashMap<String, ast::Function> = HashMap::new();
        for func in functions.clone() {
            map.insert(func.name.clone(), func);
        }

        let mut main_id = cranelift_module::FuncId::from_u32(0);
        let mut has_main = false;
        for func in functions {
            self.translate_function(func.parameters, func.return_type, func.body, &map)
                .unwrap();
            let id = self
                .module
                .declare_function(&func.name, Linkage::Export, &self.context.func.signature)
                .map_err(|e| e.to_string())?;
            if func.name == "main" {
                main_id = id.clone();
                has_main = true;
            }
            self.module
                .define_function(id, &mut self.context)
                .map_err(|e| e.to_string())?;
            self.module.clear_context(&mut self.context);
        }

        if !has_main {
            return Err(String::from("Does not have main function"));
        }

        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(main_id);

        Ok(code)
    }

    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        self.data_context.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_context)
            .map_err(|e| e.to_string())?;
        self.data_context.clear();
        self.module.finalize_definitions();
        let buffer = self.module.get_finalized_data(id);

        Ok(unsafe { std::slice::from_raw_parts(buffer.0, buffer.1) })
    }

    fn translate_function(
        &mut self,
        params: Vec<(String, types::Type)>,
        return_type: types::Type,
        statements: Vec<Expression>,
        functions: &HashMap<String, ast::Function>,
    ) -> Result<(), String> {
        for (_, ty) in &params {
            self.context.func.signature.params.push(AbiParam::new(*ty));
        }

        self.context
            .func
            .signature
            .returns
            .push(AbiParam::new(return_type));

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let variables =
            declare_variables(&mut builder, &params, return_type, &statements, entry_block);

        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
        };

        for expr in statements {
            trans.translate_expression(expr, functions);
        }

        trans.builder.finalize();

        Ok(())
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, (Variable, types::Type)>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_expression(
        &mut self,
        expression: Expression,
        functions: &HashMap<String, ast::Function>,
    ) -> Value {
        match expression {
            Expression::Literal(literal) => match literal {
                TypeLiteral::F32(val) => self.builder.ins().f32const(Ieee32::with_float(val)),
                TypeLiteral::I32(val) => self
                    .builder
                    .ins()
                    .iconst(types::I32, Imm64::new(val as i64)),
                _ => Value::from_u32(0),
            },
            Expression::Add(lhs, rhs) => {
                let ty_lhs = find_expression_type(&lhs, &mut self.variables, functions);
                let ty_rhs = find_expression_type(&rhs, &mut self.variables, functions);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs, functions);
                let rhs = self.translate_expression(*rhs, functions);

                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        types::F32 | types::F64 => self.builder.ins().fadd(lhs, rhs),
                        types::I8 | types::I16 | types::I32 | types::I64 => {
                            self.builder.ins().iadd(lhs, rhs)
                        }
                        _ => panic!("Cannot add this type"),
                    }
                } else {
                    panic!("Cannot find type of add expression");
                }
            }
            Expression::Sub(lhs, rhs) => {
                let ty_lhs = find_expression_type(&lhs, &mut self.variables, functions);
                let ty_rhs = find_expression_type(&rhs, &mut self.variables, functions);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs, functions);
                let rhs = self.translate_expression(*rhs, functions);

                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        types::F32 | types::F64 => self.builder.ins().fsub(lhs, rhs),
                        types::I8 | types::I16 | types::I32 | types::I64 => {
                            self.builder.ins().isub(lhs, rhs)
                        }
                        _ => panic!("Cannot subtract this type"),
                    }
                } else {
                    panic!("Cannot find type of sub expression");
                }
            }
            Expression::Mul(lhs, rhs) => {
                let ty_lhs = find_expression_type(&lhs, &mut self.variables, functions);
                let ty_rhs = find_expression_type(&rhs, &mut self.variables, functions);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs, functions);
                let rhs = self.translate_expression(*rhs, functions);

                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        types::F32 | types::F64 => self.builder.ins().fmul(lhs, rhs),
                        types::I8 | types::I16 | types::I32 | types::I64 => {
                            self.builder.ins().imul(lhs, rhs)
                        }
                        _ => panic!("Cannot subtract this type"),
                    }
                } else {
                    panic!("Cannot find type of mul expression");
                }
            }
            Expression::Div(lhs, rhs) => {
                let ty_lhs = find_expression_type(&lhs, &mut self.variables, functions);
                let ty_rhs = find_expression_type(&rhs, &mut self.variables, functions);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs, functions);
                let rhs = self.translate_expression(*rhs, functions);
                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        types::F32 | types::F64 => self.builder.ins().fdiv(lhs, rhs),
                        types::I8 | types::I16 | types::I32 | types::I64 => {
                            self.builder.ins().udiv(lhs, rhs)
                        }
                        _ => panic!("Cannot divide this type"),
                    }
                } else {
                    panic!("Cannot find type of div expression");
                }
            }
            Expression::Eq(lhs, rhs) => self.translate_cmp(Comparison::Eq, *lhs, *rhs, functions),
            Expression::Ne(lhs, rhs) => self.translate_cmp(Comparison::Ne, *lhs, *rhs, functions),
            Expression::Gt(lhs, rhs) => self.translate_cmp(Comparison::Gt, *lhs, *rhs, functions),
            Expression::Ge(lhs, rhs) => self.translate_cmp(Comparison::Ge, *lhs, *rhs, functions),
            Expression::Lt(lhs, rhs) => self.translate_cmp(Comparison::Lt, *lhs, *rhs, functions),
            Expression::Le(lhs, rhs) => self.translate_cmp(Comparison::Le, *lhs, *rhs, functions),
            Expression::Call(name, args) => self.translate_call(&name, args, functions),
            Expression::Return(val) => {
                let return_value;
                if let Some(expr) = val {
                    return_value = self.translate_expression(*expr, functions);
                    self.builder.ins().return_(&[return_value]);
                    return_value
                } else {
                    self.builder.ins().return_(&[]);
                    self.builder.ins().null(types::INVALID)
                }
            }
            Expression::Assign(name, expr, _ty) => self.translate_assign(&name, *expr, functions),
            Expression::Identifier(name) => {
                let var = self.variables.get(&name).expect("Variable not declared").0;
                self.builder.use_var(var)
            }
            _ => panic!("Unknown expression"),
        }
    }

    fn translate_assign(
        &mut self,
        name: &str,
        expression: Expression,
        functions: &HashMap<String, ast::Function>,
    ) -> Value {
        let new_value = self.translate_expression(expression, functions);
        let variable = self.variables.get(name).unwrap().0;
        self.builder.def_var(variable, new_value);
        new_value
    }

    fn translate_call(
        &mut self,
        name: &str,
        args: Vec<Expression>,
        functions: &HashMap<String, ast::Function>,
    ) -> Value {
        let mut signature = self.module.make_signature();
        let func = functions.get(name).unwrap();
        signature.returns.push(AbiParam::new(func.return_type));

        for (i, args) in args.iter().enumerate() {
            signature.params.push(AbiParam::new(func.parameters[i].1));
        }

        let callee = self
            .module
            .declare_function(name, Linkage::Import, &signature)
            .expect("Could not declare function");

        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.translate_expression(arg, functions));
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn translate_global_data_address(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("Could not declare reference in function");

        let local_id = self
            .module
            .declare_data_in_func(sym, &mut self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }

    fn translate_cmp(
        &mut self,
        comp: Comparison,
        lhs: Expression,
        rhs: Expression,
        functions: &HashMap<String, ast::Function>,
    ) -> Value {
        let ty_l = find_expression_type(&lhs, &mut self.variables, functions);
        let ty_r = find_expression_type(&rhs, &mut self.variables, functions);

        if ty_l != ty_r {
            panic!("Expression arms type do not match");
        }

        let cmp = {
            if ty_l.unwrap() == types::F32 || ty_l.unwrap() == types::F64 {
                match comp {
                    Comparison::Eq => Either::Left(FloatCC::Equal),
                    Comparison::Ne => Either::Left(FloatCC::NotEqual),
                    Comparison::Gt => Either::Left(FloatCC::GreaterThan),
                    Comparison::Ge => Either::Left(FloatCC::GreaterThanOrEqual),
                    Comparison::Lt => Either::Left(FloatCC::LessThan),
                    Comparison::Le => Either::Left(FloatCC::LessThanOrEqual),
                    _ => unreachable!(),
                }
            } else {
                match comp {
                    Comparison::Eq => Either::Right(IntCC::Equal),
                    Comparison::Ne => Either::Right(IntCC::NotEqual),
                    Comparison::Gt => Either::Right(IntCC::SignedGreaterThan),
                    Comparison::Ge => Either::Right(IntCC::SignedGreaterThanOrEqual),
                    Comparison::Lt => Either::Right(IntCC::SignedLessThan),
                    Comparison::Le => Either::Right(IntCC::SignedLessThanOrEqual),
                    _ => unreachable!(),
                }
            }
        };

        match cmp {
            Left(floatcc) => self.translate_fcmp(floatcc, lhs, rhs, functions),
            Right(intcc) => self.translate_icmp(intcc, lhs, rhs, functions),
        }
    }

    fn translate_icmp(
        &mut self,
        cmp: IntCC,
        lhs: Expression,
        rhs: Expression,
        functions: &HashMap<String, ast::Function>,
    ) -> Value {
        let lhs = self.translate_expression(lhs, functions);
        let rhs = self.translate_expression(rhs, functions);
        self.builder.ins().icmp(cmp, lhs, rhs)
    }
    fn translate_fcmp(
        &mut self,
        cmp: FloatCC,
        lhs: Expression,
        rhs: Expression,
        functions: &HashMap<String, ast::Function>,
    ) -> Value {
        let lhs = self.translate_expression(lhs, functions);
        let rhs = self.translate_expression(rhs, functions);
        self.builder.ins().fcmp(cmp, lhs, rhs)
    }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    params: &[(String, types::Type)],
    return_type: types::Type,
    statements: &[Expression],
    entry_block: Block,
) -> HashMap<String, (Variable, types::Type)> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, (name, ty)) in params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(*ty, Some(val), builder, &mut variables, &mut index, name);
    }

    let zero = builder.ins().iconst(types::I32, 0);

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
                None,
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
    value: Option<Value>,
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
    if let Some(val) = value {
        builder.def_var(var, val);
    } else {
        let val = match ty {
            types::F32 => builder.ins().f32const(0.0),
            types::F64 => builder.ins().f64const(0.0),
            types::I8 => builder.ins().iconst(types::I8, Imm64::new(0)),
            types::I16 => builder.ins().iconst(types::I16, Imm64::new(0)),
            types::I32 => builder.ins().iconst(types::I32, Imm64::new(0)),
            types::I64 => builder.ins().iconst(types::I64, Imm64::new(0)),
            _ => panic!("Type not implemented"),
        };
        builder.def_var(var, val);
    }

    var
}

fn find_expression_type(
    expression: &Expression,
    variables: &mut HashMap<String, (Variable, types::Type)>,
    functions: &HashMap<String, ast::Function>,
) -> Option<types::Type> {
    // All literals are of same type
    let mut binary = |rhs: &Box<Expression>, lhs: &Box<Expression>| -> Option<types::Type> {
        let left = find_expression_type(lhs, variables, functions);
        let right = find_expression_type(rhs, variables, functions);
        if let Some(l) = left
            && let Some(r) = right
        {
            if l == r {
                return left;
            } else {
                return None;
            }
        } else {
            return None;
        }
    };
    match expression {
        Expression::Literal(literal) => Some(literal.get_type()),
        Expression::Identifier(ident) => Some(variables.get(ident).unwrap().1),
        Expression::Call(ref name, ref _exprs) => Some(functions.get(name).unwrap().return_type),
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

