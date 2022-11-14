use super::ast;
use super::ast::{BooleanExpr, Comparison, Expression, TypeLiteral};
use crate::grammar;

use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::*;
use either::*;

type Functions = HashMap<String, ast::Function>;

pub struct JitCodegen {
    builder_context: FunctionBuilderContext,
    context: codegen::Context,
    data_context: DataContext,
    module: JITModule,
}

pub struct AotCodegen {
    builder_context: FunctionBuilderContext,
    context: codegen::Context,
    data_context: DataContext,
}

impl Default for JitCodegen {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            context: module.make_context(),
            data_context: DataContext::new(),
            module,
        }
    }
}

impl Default for AotCodegen {
    fn default() -> Self {
        Self {
            builder_context: FunctionBuilderContext::new(),
            context: codegen::Context::new(),
            data_context: DataContext::new(),
        }
    }
}

impl AotCodegen {
    pub fn compile_project(
        &mut self,
        main_module_path: &std::path::Path,
        output: &std::path::Path,
    ) {
        let main_code = std::fs::read_to_string(main_module_path).unwrap();

        let module_definitions: Vec<String> = grammar::mod_scan::modules(&main_code).unwrap();

        let flag_builder = settings::builder();
        let mut flags = settings::Flags::new(flag_builder);
        flags.enable_verifier();
        flags.enable_verifier();
        let isa_builder = codegen::isa::lookup_by_name("x86_64-unknown-linux-gnu").unwrap();
        let isa = isa_builder.finish(flags).unwrap();

        let mut builder = ObjectBuilder::new(
            isa,
            main_module_path.to_str().unwrap(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        builder.per_function_section(false);
        let mut module = ObjectModule::new(builder);
        self.context = module.make_context();
        let mut functions = HashMap::new();

        self.insert_libc_functions(&mut module, &mut functions);
        self.insert_start(&mut module);

        self.create_data(&mut module, "hello", "hello\0".to_string().into_bytes())
            .unwrap();

        self.compile_module(&main_code, &mut module, &mut functions)
            .unwrap();
        for include_module in module_definitions {
            let mut path = std::env::current_dir().unwrap();
            let mut include = std::path::PathBuf::from(&include_module);
            include.set_extension("jadescript");
            path.push(&include);

            let module_code = std::fs::read_to_string(path).expect("Module is not a file");

            self.compile_module(&module_code, &mut module, &mut functions)
                .unwrap();
        }

        let compiled_module = module.finish();
        println!("Functions {:?}", compiled_module.functions);

        let data = compiled_module.emit().map_err(|e| e.to_string()).unwrap();
        let mut output_path = std::env::current_dir().unwrap();
        output_path.push(output);

        println!("{:?}", output);
        std::fs::write(output_path, data).unwrap();
    }

    // Internal module, not separate compilation unit
    fn compile_module(
        &mut self,
        code: &str,
        module: &mut ObjectModule,
        functions: &mut Functions,
    ) -> Result<(), String> {
        let module_functions = grammar::parser::file(code).map_err(|e| e.to_string())?;

        for func in module_functions.clone() {
            functions.insert(func.name.clone(), func);
        }

        let mut main_id = cranelift_module::FuncId::from_u32(0);
        let mut has_main = false;
        for func in module_functions {
            self.translate_function(
                module,
                &func.name,
                func.parameters,
                func.return_type,
                func.body,
                functions,
            )
            .unwrap();
        }

        // if !has_main {
        //     return Err(String::from("Does not have main function"));
        // }

        Ok(())
    }

    fn insert_start(&mut self, module: &mut ObjectModule) {
        let sig = Signature {
            params: vec![],
            returns: vec![AbiParam::new(types::I32)],
            call_conv: isa::CallConv::SystemV,
        };
        let id = module
            .declare_function("_start", Linkage::Export, &sig)
            .unwrap();

        let func = codegen::ir::function::Function::with_name_signature(
            codegen::ir::UserFuncName::user(0, 0),
            sig,
        );
        self.context.func = func;
        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        {
            let entry_block = builder.create_block();

            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let main_sig = Signature {
                params: vec![],
                returns: vec![],
                call_conv: isa::CallConv::SystemV,
            };
            // Call main
            let callee = module
                .declare_function("main", Linkage::Import, &main_sig)
                .expect("Could not declare function");

            let local_callee = module.declare_func_in_func(callee, &mut builder.func);
            builder.ins().call(local_callee, &[]);
            let out = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[out]);

            builder.seal_all_blocks();
            builder.finalize();
            // Syscall exit
        }
        module.define_function(id, &mut self.context).unwrap();
    }

    fn translate_function(
        &mut self,
        module: &mut ObjectModule,
        name: &str,
        params: Vec<(String, types::Type)>,
        return_type: Option<types::Type>,
        statements: Vec<Expression>,
        functions: &Functions,
    ) -> Result<(), String> {
        let sig = Signature {
            params: params
                .iter()
                .map(|p| AbiParam::new(p.1))
                .collect::<Vec<_>>(),
            returns: match return_type {
                Some(ty) => vec![AbiParam::new(ty)],
                None => vec![],
            },
            call_conv: isa::CallConv::SystemV,
        };

        let linkage = match name {
            "main" => Linkage::Export,
            _ => Linkage::Local,
        };
        let id = module
            .declare_function(name, linkage, &sig)
            .map_err(|e| e.to_string())?;

        let func = codegen::ir::function::Function::with_name_signature(
            codegen::ir::UserFuncName::user(0, id.as_u32() + 1), // +1 for _start
            sig,
        );
        self.context.func = func;
        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        {
            let entry_block = builder.create_block();

            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);
            let variables = HashMap::new();

            let mut trans = FunctionTranslator {
                builder,
                variables,
                module,
            };

            for (i, (name, ty)) in params.iter().enumerate() {
                let val = trans.builder.block_params(entry_block)[i];
                let var = Variable::new(trans.variables.len());
                trans.variables.insert(name.into(), (var, *ty));
                trans.builder.declare_var(var, *ty);
                trans.builder.def_var(var, val);
            }

            for expr in statements {
                trans.translate_expression(expr, functions);
            }
            trans.builder.seal_all_blocks();
            trans.builder.finalize();
        }

        module.define_function(id, &mut self.context).unwrap();

        Ok(())
    }

    pub fn create_data(
        &mut self,
        module: &mut ObjectModule,
        name: &str,
        contents: Vec<u8>,
    ) -> Result<(), String> {
        self.data_context.define(contents.into_boxed_slice());
        let id = module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        module
            .define_data(id, &self.data_context)
            .map_err(|e| e.to_string())?;
        self.data_context.clear();
        Ok(())
    }

    fn insert_libc_functions(&mut self, module: &mut ObjectModule, functions: &mut Functions) {
        let pointer = module.target_config().pointer_type();
        let puts = ast::Function {
            name: "puts".to_string(),
            parameters: vec![(String::from("string"), pointer)],
            return_type: None,
            body: vec![],
        };

        functions.insert(String::from("puts"), puts);

        let sig = Signature {
            params: vec![AbiParam::new(pointer)],
            returns: vec![],
            call_conv: isa::CallConv::SystemV,
        };

        // module
        //     .declare_function("puts", Linkage::Import, &sig)
        //     .unwrap();
    }
}

impl JitCodegen {
    pub fn compile(&mut self, input: &str) -> Result<*const u8, String> {
        let functions = grammar::parser::file(input).map_err(|e| e.to_string())?;
        let mut map: Functions = HashMap::new();
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
                main_id = id;
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
        return_type: Option<types::Type>,
        statements: Vec<Expression>,
        functions: &Functions,
    ) -> Result<(), String> {
        for (_, ty) in &params {
            self.context.func.signature.params.push(AbiParam::new(*ty));
        }

        if let Some(ty) = return_type {
            self.context.func.signature.returns.push(AbiParam::new(ty));
        }

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let variables = HashMap::new();

        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
        };

        for (i, (name, ty)) in params.iter().enumerate() {
            let val = trans.builder.block_params(entry_block)[i];
            let var = Variable::new(trans.variables.len());
            trans.variables.insert(name.into(), (var, *ty));
            trans.builder.declare_var(var, *ty);
            trans.builder.def_var(var, val);
        }

        for expr in statements {
            trans.translate_expression(expr, functions);
        }

        trans.builder.finalize();

        Ok(())
    }
}

struct FunctionTranslator<'a, T: Module> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, (Variable, types::Type)>,
    module: &'a mut T,
}

impl<'a, T: Module> FunctionTranslator<'a, T> {
    fn translate_expression(&mut self, expression: Expression, functions: &Functions) -> Value {
        match expression {
            Expression::Literal(literal) => match literal {
                TypeLiteral::F32(val) => self.builder.ins().f32const(Ieee32::with_float(val)),
                TypeLiteral::I32(val) => self
                    .builder
                    .ins()
                    .iconst(types::I32, Imm64::new(val as i64)),
                TypeLiteral::Bool(val) => self.builder.ins().bconst(types::B8, val),
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
            Expression::And(lhs, rhs) => {
                self.translate_boolean_expression(*lhs, *rhs, BooleanExpr::And, functions)
            }
            Expression::Or(lhs, rhs) => {
                self.translate_boolean_expression(*lhs, *rhs, BooleanExpr::Or, functions)
            }
            Expression::Xor(lhs, rhs) => {
                self.translate_boolean_expression(*lhs, *rhs, BooleanExpr::Xor, functions)
            }
            Expression::Not(expr) => {
                self.translate_boolean_unary(*expr, BooleanExpr::Not, functions)
            }
            Expression::Eq(lhs, rhs) => self.translate_cmp(Comparison::Eq, *lhs, *rhs, functions),
            Expression::Ne(lhs, rhs) => self.translate_cmp(Comparison::Ne, *lhs, *rhs, functions),
            Expression::Gt(lhs, rhs) => self.translate_cmp(Comparison::Gt, *lhs, *rhs, functions),
            Expression::Ge(lhs, rhs) => self.translate_cmp(Comparison::Ge, *lhs, *rhs, functions),
            Expression::Lt(lhs, rhs) => self.translate_cmp(Comparison::Lt, *lhs, *rhs, functions),
            Expression::Le(lhs, rhs) => self.translate_cmp(Comparison::Le, *lhs, *rhs, functions),
            Expression::Call(name, args, base) => self.translate_call(&name, args, base, functions),
            Expression::Return(val) => {
                let return_value;
                if let Some(expr) = val {
                    return_value = self.translate_expression(*expr, functions);
                    self.builder.ins().return_(&[return_value]);
                    return_value
                } else {
                    self.builder.ins().return_(&[]);
                    Value::from_u32(0)
                }
            }
            Expression::Assign(name, expr) => self.translate_assign(&name, *expr, functions),
            Expression::Declare(name, expr, ty, _mutable) => {
                self.translate_declare(&name, *expr, ty, functions)
            }
            Expression::Identifier(name) => {
                let var = self.variables.get(&name).expect("Variable not declared").0;
                self.builder.use_var(var)
            }
            Expression::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body, functions)
            }
            Expression::While(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body, functions)
            }
            Expression::GlobalDataAddr(addr) => self.translate_global_data_address(addr),
            _ => panic!("Unknown expression"),
        }
    }

    fn translate_assign(
        &mut self,
        name: &str,
        expression: Expression,
        functions: &Functions,
    ) -> Value {
        let new_value = self.translate_expression(expression, functions);
        let variable = self.variables.get(name).unwrap().0;
        self.builder.def_var(variable, new_value);
        new_value
    }

    fn translate_declare(
        &mut self,
        name: &str,
        expression: Expression,
        type_hint: Option<types::Type>,
        functions: &Functions,
    ) -> Value {
        let ty;

        if let Some(type_) = type_hint {
            ty = type_;
        } else {
            ty = find_expression_type(&expression, &self.variables, functions)
                .expect("Could not determine expression type");
        }

        let var = Variable::new(self.variables.len());
        self.variables.insert(name.into(), (var, ty));
        self.builder.declare_var(var, ty);

        let value = self.translate_expression(expression, functions);
        self.builder.def_var(var, value);
        value
    }

    fn translate_call(
        &mut self,
        name: &str,
        args: Vec<Expression>,
        base: Option<Box<Expression>>,
        functions: &Functions,
    ) -> Value {
        let mut signature = self.module.make_signature();

        let func = functions
            .get(name)
            .expect(&format!("Function '{}' not declared", name));

        if let Some(ty) = func.return_type {
            signature.returns.push(AbiParam::new(ty));
        }

        for (i, _args) in func.parameters.iter().enumerate() {
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
        if let Some(b) = base {
            arg_values.push(self.translate_expression(*b, functions));
        }

        for arg in args {
            arg_values.push(self.translate_expression(arg, functions));
        }

        let call = self.builder.ins().call(local_callee, &arg_values);

        if func.return_type.is_some() {
            self.builder.inst_results(call)[0]
        } else {
            Value::from_u32(0)
        }
    }

    fn translate_if_else(
        &mut self,
        cond: Expression,
        then_body: Vec<Expression>,
        else_body: Vec<Expression>,
        functions: &Functions,
    ) -> Value {
        let cond_type = find_expression_type(&cond, &self.variables, functions).unwrap();
        let condition_value = self.translate_expression(cond, functions);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        if cond_type != types::B8 {
            unimplemented!("If condition must be a boolean expression");
        }

        let then_ty = find_expression_type(
            then_body.last().expect("If body has no expressions"),
            &self.variables,
            functions,
        );

        let else_ty = find_expression_type(
            else_body.last().expect("Else body has no expressions"),
            &self.variables,
            functions,
        );

        if else_ty.is_some() && then_ty.is_some() && then_ty.unwrap() != else_ty.unwrap() {
            panic!("If body and else body do not return the same type");
        }

        // Blocks return the last expression
        if let Some(ty) = then_ty {
            self.builder.append_block_param(merge_block, ty);
        } else {
            self.builder.append_block_param(merge_block, types::I32);
        }

        self.builder.ins().brz(condition_value, else_block, &[]);
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let mut return_value;
        if let Some(ty) = then_ty {
            return_value = self.default_value(ty);
        } else {
            return_value = self.builder.ins().iconst(types::I32, 0);
        }

        let mut has_return = false;
        for expr in then_body {
            if let Expression::Return(_) = expr {
                has_return = true;
            }
            return_value = self.translate_expression(expr, functions);
        }

        // If the body has a return then don't add another jump
        if !has_return {
            self.builder.ins().jump(merge_block, &[return_value]);
        }

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        let mut else_return_value;
        if let Some(ty) = then_ty {
            else_return_value = self.default_value(ty);
        } else {
            else_return_value = self.builder.ins().iconst(types::I32, 0);
        }
        let mut has_else_return = false;
        for expr in else_body {
            if let Expression::Return(_) = &expr {
                has_else_return = true;
            }
            else_return_value = self.translate_expression(expr, functions);
        }

        // If the body has a return then don't add another jump
        if !has_else_return {
            self.builder.ins().jump(merge_block, &[else_return_value]);
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        let phi = self.builder.block_params(merge_block)[0];

        phi
    }

    fn translate_while_loop(
        &mut self,
        condition: Expression,
        loop_body: Vec<Expression>,
        functions: &Functions,
    ) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_ty = find_expression_type(&condition, &self.variables, functions);
        if condition_ty.unwrap() != types::B8 {
            panic!("While loop condition must be a boolean expression");
        }

        let condition_value = self.translate_expression(condition, functions);
        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        for expr in loop_body {
            self.translate_expression(expr, functions);
        }

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);

        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        self.builder.ins().iconst(types::I32, 0)
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

    fn translate_boolean_expression(
        &mut self,
        lhs: Expression,
        rhs: Expression,
        op: BooleanExpr,
        functions: &Functions,
    ) -> Value {
        let lhs_ty = find_expression_type(&lhs, &self.variables, functions);
        let rhs_ty = find_expression_type(&rhs, &self.variables, functions);

        if let Some(lt) = lhs_ty
            && let Some(rt) = rhs_ty
        {
            if lt != types::B8 || rt != types::B8 {
                panic!("Binary boolean expression must use boolean type");
            }
        } else {
            panic!("Binary boolean expressions type do not match");
        }

        let rhs = self.translate_expression(rhs, functions);
        let lhs = self.translate_expression(lhs, functions);

        match op {
            BooleanExpr::Or => self.builder.ins().bor(rhs, lhs),
            BooleanExpr::Xor => self.builder.ins().bxor(rhs, lhs),
            BooleanExpr::And => self.builder.ins().band(rhs, lhs),
            _ => unimplemented!(),
        }
    }

    fn translate_boolean_unary(
        &mut self,
        expr: Expression,
        op: BooleanExpr,
        functions: &Functions,
    ) -> Value {
        let ty = find_expression_type(&expr, &self.variables, functions);

        if let Some(expr_ty) = ty {
            if expr_ty != types::B8 {
                panic!("Unary boolean expression must use boolean type");
            }
        }

        let expr = self.translate_expression(expr, functions);

        match op {
            BooleanExpr::Not => self.builder.ins().bnot(expr),
            _ => unimplemented!(),
        }
    }
    fn translate_cmp(
        &mut self,
        comp: Comparison,
        lhs: Expression,
        rhs: Expression,
        functions: &Functions,
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
                }
            } else {
                match comp {
                    Comparison::Eq => Either::Right(IntCC::Equal),
                    Comparison::Ne => Either::Right(IntCC::NotEqual),
                    Comparison::Gt => Either::Right(IntCC::SignedGreaterThan),
                    Comparison::Ge => Either::Right(IntCC::SignedGreaterThanOrEqual),
                    Comparison::Lt => Either::Right(IntCC::SignedLessThan),
                    Comparison::Le => Either::Right(IntCC::SignedLessThanOrEqual),
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
        functions: &Functions,
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
        functions: &Functions,
    ) -> Value {
        let lhs = self.translate_expression(lhs, functions);
        let rhs = self.translate_expression(rhs, functions);
        self.builder.ins().fcmp(cmp, lhs, rhs)
    }

    fn default_value(&mut self, ty: types::Type) -> Value {
        match ty {
            types::F32 => self.builder.ins().f32const(0.0),
            types::F64 => self.builder.ins().f64const(0.0),
            types::I8 => self.builder.ins().iconst(types::I8, 0),
            types::I16 => self.builder.ins().iconst(types::I16, 0),
            types::I32 => self.builder.ins().iconst(types::I32, 0),
            types::I64 => self.builder.ins().iconst(types::I64, 0),
            types::B8 => self.builder.ins().bconst(types::B8, false),
            _ => unimplemented!(),
        }
    }
}

fn find_expression_type(
    expression: &Expression,
    variables: &HashMap<String, (Variable, types::Type)>,
    functions: &Functions,
) -> Option<types::Type> {
    // All literals are of same type
    let binary = |rhs: &Box<Expression>, lhs: &Box<Expression>| -> Option<types::Type> {
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
        Expression::Call(ref name, ref _exprs, _) => functions.get(name).unwrap().return_type,
        Expression::Eq(_, _)
        | Expression::Ne(_, _)
        | Expression::Lt(_, _)
        | Expression::Le(_, _)
        | Expression::Gt(_, _)
        | Expression::Ge(_, _)
        | Expression::And(_, _)
        | Expression::Or(_, _)
        | Expression::Xor(_, _)
        | Expression::Not(_) => Some(types::B8),
        Expression::Add(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Sub(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Mul(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Div(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Assign(_, _) => None,
        Expression::Return(ref _expr) => None,
        _ => None,
    }
}

