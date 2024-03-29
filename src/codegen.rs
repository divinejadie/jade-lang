use super::ast;
use super::ast::{BooleanExpr, Comparison, Expression, TypeLiteral};
use crate::parser;

use std::collections::HashMap;

use cranelift::codegen::ir::StackSlot;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, FuncId, Linkage, Module};
use cranelift_object::*;
use either::*;
use once_cell::sync::OnceCell;

type Functions = HashMap<String, ast::Function>;
type Structs = HashMap<String, ast::Struct>;

static JIT_ENTRY: OnceCell<FuncId> = OnceCell::new();
const FILE_EXT: &'static str = "jade";

const BUILTINS: [&'static str; 2] = ["sqrt", "abs"];

pub struct JitCodegen {
    builder_context: FunctionBuilderContext,
    context: codegen::Context,
    data_context: DataContext,
    module: JITModule,
}

pub struct AotCodegen {
    builder_context: FunctionBuilderContext,
    context: codegen::Context,
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
        }
    }
}

// Internal module, not separate compilation unit
fn compile_module<T: Module>(
    dir: &std::path::Path,
    code: &str,
    builder_context: &mut FunctionBuilderContext,
    context: &mut codegen::Context,
    module: &mut T,
    functions: &mut Functions,
    structs: &mut Structs,
) -> Result<(), String> {
    let source: Vec<ast::SourceFileItem> = parser::parse(code).map_err(|e| e.to_string())?;

    let mut module_functions = vec![];
    let mut declared_modules = vec![];

    for i in source {
        match &i {
            ast::SourceFileItem::Module(m) => declared_modules.push(m.clone()),
            ast::SourceFileItem::Struct(s) => {
                structs.insert(s.name.clone(), s.clone());
            }
            ast::SourceFileItem::Function(f) => {
                functions.insert(f.name.clone(), f.clone());
                module_functions.push(f.clone());
            }
        }
    }

    // Compile all declared modules before this one
    for include_module in declared_modules {
        let mut path = dir.to_path_buf();
        let include = std::path::PathBuf::from(&include_module);
        path.push(&include);

        if path.is_dir() {
            path.push("mod");
            path.set_extension(FILE_EXT);

            if !path.exists() {
                return Err(format!(
                    "Module {} is declared, but it's folder is empty",
                    include_module
                ));
            }
        } else {
            path.set_extension(FILE_EXT);
        }
        // Modules can either be a single file, OR a folder with <mod_name>/mod.jade

        let module_code = std::fs::read_to_string(&path)
            .expect(&format!("Failed reading module {}", include_module));

        let dir = path.parent();

        compile_module(
            &dir.unwrap(),
            &module_code,
            builder_context,
            context,
            module,
            functions,
            structs,
        )
        .unwrap();
    }

    for func in module_functions {
        translate_function(builder_context, context, module, func, &functions, &structs)?;
    }

    Ok(())
}

fn translate_function<T: Module>(
    builder_context: &mut FunctionBuilderContext,
    context: &mut codegen::Context,
    module: &mut T,
    function: ast::Function,
    functions: &Functions,
    structs: &Structs,
) -> Result<(), String> {
    let mut sig = module.make_signature();
    if let Some(ty) = function.return_type {
        sig.returns.push(AbiParam::new(ty.to_ir(module.isa())));
    }

    for param in &function.parameters {
        sig.params.push(AbiParam::new(param.1.to_ir(module.isa())));
    }

    let linkage = Linkage::Local;

    let id = module
        .declare_function(&function.name, linkage, &sig)
        .map_err(|e| e.to_string())?;

    if function.name == "main" {
        if let Err(_) = JIT_ENTRY.set(id.clone()) {
            log::error!("Multiple main() entrypoints")
        }
    }

    let func = codegen::ir::function::Function::with_name_signature(
        codegen::ir::UserFuncName::user(0, id.as_u32() + 1), // +1 for _start
        sig,
    );
    context.func = func;
    let mut builder = FunctionBuilder::new(&mut context.func, builder_context);
    {
        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let variables = HashMap::new();

        let mut trans = FunctionTranslator {
            builder,
            variables,
            stack_slots: HashMap::new(),
            functions,
            structs,
            module,
        };

        for (i, (name, ty)) in function.parameters.iter().enumerate() {
            let val = trans.builder.block_params(entry_block)[i];
            let var = Variable::new(trans.variables.len());
            trans.variables.insert(name.into(), (var, ty.clone()));
            trans.builder.declare_var(var, ty.to_ir(trans.module.isa()));
            trans.builder.def_var(var, val);
        }

        for expr in function.body {
            trans.translate_expression(expr);
        }
        trans.builder.seal_all_blocks();
        trans.builder.finalize();
    }

    module.define_function(id, context).unwrap();
    module.clear_context(context);

    Ok(())
}

pub fn create_data<T: Module>(
    data_context: &mut DataContext,
    module: &mut T,
    name: &str,
    contents: Vec<u8>,
) -> Result<(), String> {
    data_context.define(contents.into_boxed_slice());
    let id = module
        .declare_data(name, Linkage::Export, true, false)
        .map_err(|e| e.to_string())?;

    module
        .define_data(id, data_context)
        .map_err(|e| e.to_string())?;
    data_context.clear();
    Ok(())
}

fn insert_libc_functions(module: &mut dyn Module, functions: &mut Functions) {
    let mut sig = module.make_signature();
    let pointer = module.target_config().pointer_type();
    sig.params.push(AbiParam::new(pointer));

    let puts = ast::Function {
        name: "puts".to_string(),
        parameters: vec![(String::from("string"), ast::Type::Pointer)],
        return_type: None,
        body: vec![],
    };

    functions.insert(String::from("puts"), puts);
}

impl AotCodegen {
    pub fn compile_project(
        &mut self,
        main_module_path: &std::path::Path,
        output: &std::path::Path,
    ) {
        let main_code = std::fs::read_to_string(main_module_path).unwrap();

        let flag_builder = settings::builder();
        let flags = settings::Flags::new(flag_builder);
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
        let mut structs = HashMap::new();

        insert_libc_functions(&mut module, &mut functions);
        self.insert_start(&mut module);

        let src_dir = main_module_path.parent().unwrap();

        compile_module(
            &src_dir,
            &main_code,
            &mut self.builder_context,
            &mut self.context,
            &mut module,
            &mut functions,
            &mut structs,
        )
        .unwrap();

        let compiled_module = module.finish();

        let data = compiled_module.emit().map_err(|e| e.to_string()).unwrap();
        let mut output_path = std::env::current_dir().unwrap();
        output_path.push(output);

        std::fs::write(output_path, data).unwrap();
    }

    fn insert_start(&mut self, module: &mut ObjectModule) {
        let mut sig = module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));

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

            let main_sig = module.make_signature();
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
}

impl JitCodegen {
    pub fn compile(&mut self, input: &str) -> Result<*const u8, String> {
        let main_code = input;

        let builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();
        let mut module = JITModule::new(builder);
        self.context = module.make_context();

        let mut functions = HashMap::new();
        let mut structs = HashMap::new();

        insert_libc_functions(&mut module, &mut functions);

        compile_module(
            &std::path::PathBuf::new(),
            &main_code,
            &mut self.builder_context,
            &mut self.context,
            &mut module,
            &mut functions,
            &mut structs,
        )
        .unwrap();

        module.clear_context(&mut self.context);
        module.finalize_definitions().map_err(|x| x.to_string())?;

        let code = module.get_finalized_function(*JIT_ENTRY.get().unwrap());

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
        self.module
            .finalize_definitions()
            .map_err(|x| x.to_string())?;
        let buffer = self.module.get_finalized_data(id);

        Ok(unsafe { std::slice::from_raw_parts(buffer.0, buffer.1) })
    }
}

struct FunctionTranslator<'a, T: Module> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, (Variable, ast::Type)>,
    stack_slots: HashMap<String, StackSlot>,
    functions: &'a Functions,
    structs: &'a Structs,
    module: &'a mut T,
}

impl<'a, T: Module> FunctionTranslator<'a, T> {
    fn translate_expression(&mut self, expression: Expression) -> Value {
        match expression {
            Expression::Literal(literal) => self.translate_literal(literal),
            Expression::Add(lhs, rhs) => {
                let ty_lhs =
                    find_expression_type(&lhs, &self.variables, self.functions, self.structs);
                let ty_rhs =
                    find_expression_type(&rhs, &self.variables, self.functions, self.structs);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs);
                let rhs = self.translate_expression(*rhs);

                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        ast::Type::F32 | ast::Type::F64 => self.builder.ins().fadd(lhs, rhs),
                        ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::I64 => {
                            self.builder.ins().iadd(lhs, rhs)
                        }
                        ast::Type::U8 | ast::Type::U16 | ast::Type::U32 | ast::Type::U64 => self
                            .builder
                            .ins()
                            .uadd_overflow_trap(lhs, rhs, TrapCode::IntegerOverflow),
                        _ => panic!("Cannot add this type"),
                    }
                } else {
                    panic!("Cannot find type of add expression");
                }
            }
            Expression::Sub(lhs, rhs) => {
                let ty_lhs =
                    find_expression_type(&lhs, &self.variables, self.functions, self.structs);
                let ty_rhs =
                    find_expression_type(&rhs, &self.variables, self.functions, self.structs);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs);
                let rhs = self.translate_expression(*rhs);

                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        ast::Type::F32 | ast::Type::F64 => self.builder.ins().fsub(lhs, rhs),
                        ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::I64 => {
                            self.builder.ins().isub(lhs, rhs)
                        }
                        ast::Type::U8 | ast::Type::U16 | ast::Type::U32 | ast::Type::U64 => {
                            self.builder.ins().isub(lhs, rhs)
                        }
                        _ => panic!("Cannot subtract this type"),
                    }
                } else {
                    panic!("Cannot find type of sub expression");
                }
            }
            Expression::Mul(lhs, rhs) => {
                let ty_lhs =
                    find_expression_type(&lhs, &self.variables, self.functions, self.structs);
                let ty_rhs =
                    find_expression_type(&rhs, &self.variables, self.functions, self.structs);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs);
                let rhs = self.translate_expression(*rhs);

                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        ast::Type::F32 | ast::Type::F64 => self.builder.ins().fmul(lhs, rhs),
                        ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::I64 => {
                            self.builder.ins().imul(lhs, rhs)
                        }
                        ast::Type::U8 | ast::Type::U16 | ast::Type::U32 | ast::Type::U64 => {
                            self.builder.ins().imul(lhs, rhs)
                        }
                        _ => panic!("Cannot subtract this type"),
                    }
                } else {
                    panic!("Cannot find type of mul expression");
                }
            }
            Expression::Div(lhs, rhs) => {
                let ty_lhs =
                    find_expression_type(&lhs, &self.variables, self.functions, self.structs);
                let ty_rhs =
                    find_expression_type(&rhs, &self.variables, self.functions, self.structs);

                if ty_lhs != ty_rhs {
                    panic!("Cannot add two different types");
                }

                let lhs = self.translate_expression(*lhs);
                let rhs = self.translate_expression(*rhs);
                if let Some(expr_type) = ty_lhs {
                    match expr_type {
                        ast::Type::F32 | ast::Type::F64 => self.builder.ins().fdiv(lhs, rhs),
                        ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::I64 => {
                            self.builder.ins().sdiv(lhs, rhs)
                        }
                        ast::Type::U8 | ast::Type::U16 | ast::Type::U32 | ast::Type::U64 => {
                            self.builder.ins().udiv(lhs, rhs)
                        }
                        _ => panic!("Cannot divide this type"),
                    }
                } else {
                    panic!("Cannot find type of div expression");
                }
            }
            Expression::And(lhs, rhs) => {
                self.translate_boolean_expression(*lhs, *rhs, BooleanExpr::And)
            }
            Expression::Or(lhs, rhs) => {
                self.translate_boolean_expression(*lhs, *rhs, BooleanExpr::Or)
            }
            Expression::Xor(lhs, rhs) => {
                self.translate_boolean_expression(*lhs, *rhs, BooleanExpr::Xor)
            }
            Expression::Not(expr) => self.translate_boolean_unary(*expr, BooleanExpr::Not),
            Expression::Eq(lhs, rhs) => self.translate_cmp(Comparison::Eq, *lhs, *rhs),
            Expression::Ne(lhs, rhs) => self.translate_cmp(Comparison::Ne, *lhs, *rhs),
            Expression::Gt(lhs, rhs) => self.translate_cmp(Comparison::Gt, *lhs, *rhs),
            Expression::Ge(lhs, rhs) => self.translate_cmp(Comparison::Ge, *lhs, *rhs),
            Expression::Lt(lhs, rhs) => self.translate_cmp(Comparison::Lt, *lhs, *rhs),
            Expression::Le(lhs, rhs) => self.translate_cmp(Comparison::Le, *lhs, *rhs),
            Expression::Call(name, args, base) => self.translate_call(&name, args, base),
            Expression::Return(val) => {
                let return_value;
                if let Some(expr) = val {
                    return_value = self.translate_expression(*expr);
                    self.builder.ins().return_(&[return_value]);
                    return_value
                } else {
                    self.builder.ins().return_(&[]);
                    Value::from_u32(0)
                }
            }
            Expression::Assign(name, expr) => self.translate_assign(&name, *expr),
            Expression::Declare(name, expr, ty, _mutable) => {
                self.translate_declare(&name, *expr, ty)
            }
            Expression::Identifier(name) => {
                let var = self.variables.get(&name).expect("Variable not declared").0;
                self.builder.use_var(var)
            }
            Expression::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body)
            }
            Expression::While(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
            Expression::GlobalDataAddr(addr) => self.translate_global_data_address(addr),
            Expression::StructInstantiate(ident, fields) => {
                self.translate_struct_inst(&ident, fields)
            }
            Expression::StructMember(expr, member_ident) => {
                self.translate_struct_member(&member_ident, *expr)
            }
            Expression::Cast(expr, ty) => self.translate_cast(*expr, ty),
            Expression::Array(exprs) => self.translate_array(exprs),
        }
    }

    fn translate_array(&mut self, entries: Vec<Expression>) -> Value {
        // Check all expressions evaluate to the same type
        let mut types = entries
            .iter()
            .map(|x| find_expression_type(x, &self.variables, self.functions, self.structs))
            .collect::<Vec<_>>();
        let ty = types[0].clone().unwrap();
        let eq = types.iter().all(|x| x.clone().unwrap() == ty);
        if !eq {
            panic!("Array elements are not of a single type");
        }

        let values = entries
            .into_iter()
            .map(|x| self.translate_expression(x))
            .collect::<Vec<_>>();

        let ir_type = types[0].take().unwrap().to_ir(self.module.isa());

        // <len> <[values]>
        let pointer = self.module.target_config().pointer_type();
        let slot = self
            .builder
            .create_sized_stack_slot(codegen::ir::StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: ir_type.bytes() * values.len() as u32 + pointer.bytes(),
            });

        let stack_pointer = self.builder.ins().stack_addr(pointer, slot, 0);
        let array_length = self.builder.ins().iconst(pointer, values.len() as i64);
        self.builder.ins().stack_store(array_length, slot, 0);

        for (i, val) in values.into_iter().enumerate() {
            self.builder
                .ins()
                .stack_store(val, slot, i as i32 + pointer.bytes() as i32);
        }

        stack_pointer
    }

    fn translate_cast(&mut self, expression: Expression, target_type: ast::Type) -> Value {
        let current_type =
            find_expression_type(&expression, &self.variables, self.functions, self.structs)
                .unwrap();
        let value = self.translate_expression(expression);

        // Notes:
        // Float -> Int conversions are done by rounding
        match current_type {
            ast::Type::F32 => match target_type {
                ast::Type::F64 => self.builder.ins().fpromote(types::F64, value),
                ast::Type::I32 => {
                    let rounded = self.builder.ins().nearest(value);
                    self.builder.ins().fcvt_to_sint(types::I32, rounded)
                }
                ast::Type::I64 => {
                    let rounded = self.builder.ins().nearest(value);
                    self.builder.ins().fcvt_to_sint(types::I64, rounded)
                }
                _ => unimplemented!(),
            },
            ast::Type::I32 => match target_type {
                ast::Type::I64 => self.builder.ins().sextend(types::I64, value),
                ast::Type::F32 => self.builder.ins().fcvt_from_sint(types::F32, value),
                ast::Type::F64 => self.builder.ins().fcvt_from_sint(types::F64, value),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn translate_literal(&mut self, literal: TypeLiteral) -> Value {
        match literal {
            TypeLiteral::F32(val) => self.builder.ins().f32const(Ieee32::with_float(val)),
            TypeLiteral::I32(val) => self
                .builder
                .ins()
                .iconst(types::I32, Imm64::new(val as i64)),
            TypeLiteral::U32(val) => self.builder.ins().iconst(
                types::I32,
                Imm64::new(unsafe { std::mem::transmute::<u32, i32>(val) as i64 }),
            ),
            TypeLiteral::Bool(val) => self.builder.ins().iconst(
                types::I8,
                match val {
                    true => i64::MAX, // all ones
                    false => 0i64,    // all zeros
                },
            ),
            TypeLiteral::String(val) => {
                let mut with_null = val;
                with_null.push('\0');

                let string_data = with_null.bytes();
                // Store a string onto the stack, returning a pointer to it's address.
                // TODO: Change to string span instead of null terminated.
                let pointer = self.module.target_config().pointer_type();
                let slot = self
                    .builder
                    .create_sized_stack_slot(codegen::ir::StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size: string_data.len() as u32 - 1,
                    });

                let stack_pointer = self.builder.ins().stack_addr(pointer, slot, 0);
                let byte_length = self.builder.ins().iconst(pointer, string_data.len() as i64);
                self.builder.ins().stack_store(byte_length, slot, 0);

                for (i, byte) in string_data.into_iter().enumerate() {
                    let byte = unsafe {
                        self.builder
                            .ins()
                            .iconst(types::I8, std::mem::transmute::<u8, i8>(byte) as i64)
                    };

                    self.builder.ins().stack_store(byte, slot, i as i32);
                }

                stack_pointer
            }
            _ => Value::from_u32(0),
        }
    }

    fn translate_struct_member(&mut self, member_ident: &str, expression: Expression) -> Value {
        let expr_ty =
            find_expression_type(&expression, &self.variables, &self.functions, self.structs);

        if let Some(ty) = expr_ty {
            let pointer = self.translate_expression(expression);

            if let ast::Type::Struct(name) = &ty {
                let struct_def = self
                    .structs
                    .get(name)
                    .expect(&format!("Struct with name '{}' does not exist", name));

                let member_type = struct_def.fields.get(member_ident).expect(&format!(
                    "Struct {} does not have a member variable '{}'",
                    &struct_def.name, member_ident
                ));

                let mut offsets = struct_def
                    .fields
                    .iter()
                    .map(|field| field.1.to_ir(self.module.isa()).bytes() as i32)
                    .collect::<Vec<_>>();
                // Adjust list to represent offsets instead of sizes
                offsets.insert(0, 0i32);
                offsets.pop();

                let idx = struct_def
                    .fields
                    .iter()
                    .enumerate()
                    .find_map(
                        |(idx, f)| {
                            if f.0 == member_ident {
                                Some(idx)
                            } else {
                                None
                            }
                        },
                    )
                    .unwrap();

                let offset = offsets[idx as usize];
                let flags = MemFlags::new();

                let val = self.builder.ins().load(
                    member_type.to_ir(self.module.isa()),
                    flags,
                    pointer,
                    offset,
                );

                return val;
            }
        }
        panic!("struct member");
    }

    fn translate_struct_inst(
        &mut self,
        struct_identifier: &str,
        fields: Vec<(String, Expression)>,
    ) -> Value {
        // Check if there is a struct with given name with correct type
        if let Some(struct_) = self.structs.get(struct_identifier) {
            fields.iter().for_each(|(ident, expr)| {
                let valid_name = struct_
                    .fields
                    .keys()
                    .into_iter()
                    .fold(false, |acc, x| *ident == *x || acc);
                if !valid_name {
                    panic!(
                        "Instantiated struct member {} does not exist in {}",
                        ident, struct_.name
                    );
                }

                let expr_type =
                    find_expression_type(expr, &self.variables, self.functions, self.structs)
                        .unwrap();
                if *struct_.fields.get(ident).unwrap() != expr_type {
                    panic!("Instantiated struct member {} is of wrong type", ident);
                }
            });

            // Allocate stack space for struct
            // TODO: Proper padding
            let struct_size = struct_.fields.iter().fold(0u32, |acc, (_, ty)| {
                acc + ty.to_ir(self.module.isa()).bytes()
            });

            let slot = self
                .builder
                .create_sized_stack_slot(codegen::ir::StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: struct_size,
                });
            let pointer = self.module.target_config().pointer_type();
            let instance_pointer = self.builder.ins().stack_addr(pointer, slot, 0);

            // Initialize members
            let mut offset = 0;
            for (name, expr) in fields {
                let value = self.translate_expression(expr);
                self.builder.ins().stack_store(value, slot, offset as i32);
                offset = struct_
                    .fields
                    .get(&name)
                    .unwrap()
                    .to_ir(self.module.isa())
                    .bytes();
            }

            self.stack_slots.insert(struct_identifier.to_string(), slot);

            return instance_pointer;
        } else {
            panic!("Struct with name {} not found", struct_identifier);
        }
    }

    fn translate_assign(&mut self, name: &str, expression: Expression) -> Value {
        let new_value = self.translate_expression(expression);
        let variable = self.variables.get(name).unwrap().0;
        self.builder.def_var(variable, new_value);
        new_value
    }

    fn translate_declare(
        &mut self,
        name: &str,
        expression: Expression,
        type_hint: Option<ast::Type>,
    ) -> Value {
        let ty;

        if let Some(type_) = type_hint {
            ty = type_;
        } else {
            ty = find_expression_type(&expression, &self.variables, self.functions, self.structs)
                .expect("Could not determine expression type");
        }

        let var = Variable::new(self.variables.len());
        self.variables.insert(name.into(), (var, ty.clone()));
        self.builder.declare_var(var, ty.to_ir(self.module.isa()));

        let value = self.translate_expression(expression);
        self.builder.def_var(var, value);
        value
    }

    fn translate_builtin(
        &mut self,
        name: &str,
        args: Vec<Expression>,
        base: Option<Box<Expression>>,
    ) -> Value {
        let mut args_val = Vec::<Value>::new();
        let mut args_ty = Vec::<ast::Type>::new();

        if let Some(b) = &base {
            args_ty.push(
                find_expression_type(&b, &self.variables, self.functions, self.structs).unwrap(),
            );
        }

        for arg in &args {
            args_ty.push(
                find_expression_type(&arg, &self.variables, self.functions, self.structs).unwrap(),
            );
        }

        if let Some(b) = base {
            args_val.push(self.translate_expression(*b));
        }

        for arg in args {
            args_val.push(self.translate_expression(arg));
        }

        match name {
            // Floating point square root
            "sqrt" => {
                assert_eq!(args_val.len(), 1);
                match args_ty[0] {
                    ast::Type::F32 | ast::Type::F64 => self.builder.ins().sqrt(args_val[0]),
                    _ => unimplemented!(),
                }
            }
            "abs" => {
                assert_eq!(args_val.len(), 1);
                match args_ty[0] {
                    ast::Type::F32 | ast::Type::F64 => self.builder.ins().fabs(args_val[0]),
                    ast::Type::I8 | ast::Type::I16 | ast::Type::I32 | ast::Type::I64 => {
                        todo!();
                        // self.builder.ins().iabs(args_val[0])
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn translate_call(
        &mut self,
        name: &str,
        args: Vec<Expression>,
        base: Option<Box<Expression>>,
    ) -> Value {
        if BUILTINS.contains(&name) {
            return self.translate_builtin(name, args, base);
        }

        let mut signature = self.module.make_signature();

        let func = &self
            .functions
            .get(name)
            .expect(&format!("Function '{}' not declared", name));

        if let Some(ty) = &func.return_type {
            signature
                .returns
                .push(AbiParam::new(ty.to_ir(self.module.isa())));
        }

        for (i, _args) in func.parameters.iter().enumerate() {
            signature
                .params
                .push(AbiParam::new(func.parameters[i].1.to_ir(self.module.isa())));
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
            arg_values.push(self.translate_expression(*b));
        }

        for arg in args {
            arg_values.push(self.translate_expression(arg));
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
        else_body: Option<Vec<Expression>>,
    ) -> Value {
        let cond_type =
            find_expression_type(&cond, &self.variables, self.functions, self.structs).unwrap();
        let condition_value = self.translate_expression(cond);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        if cond_type != ast::Type::Bool {
            unimplemented!("If condition must be a boolean expression");
        }

        let then_ty = find_expression_type(
            then_body.last().expect("If body has no expressions"),
            &self.variables,
            self.functions,
            self.structs,
        );

        if let Some(el) = &else_body {
            let else_ty = find_expression_type(
                el.last().expect("Else body has no expressions"),
                &self.variables,
                self.functions,
                self.structs,
            );

            if let Some(l) = &then_ty &&
            let Some(r) = &else_ty {
                if l != r {
                 panic!("If body and else body do not return the same type");
                }
            }
        }

        // Blocks return the last expression
        if let Some(ty) = &then_ty {
            self.builder
                .append_block_param(merge_block, ty.to_ir(self.module.isa()));
        } else {
            self.builder.append_block_param(merge_block, types::I32);
        }

        self.builder.ins().brz(condition_value, else_block, &[]);
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        let mut return_value;
        if let Some(ty) = &then_ty {
            return_value = self.default_value(ty);
        } else {
            return_value = self.builder.ins().iconst(types::I32, 0);
        }

        let mut has_return = false;
        for expr in then_body {
            if let Expression::Return(_) = expr {
                has_return = true;
            }
            return_value = self.translate_expression(expr);
        }

        // If the body has a return then don't add another jump
        if !has_return {
            self.builder.ins().jump(merge_block, &[return_value]);
        }

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        let mut else_return_value;
        if let Some(ty) = &then_ty {
            else_return_value = self.default_value(ty);
        } else {
            else_return_value = self.builder.ins().iconst(types::I32, 0);
        }
        let mut has_else_return = false;

        if let Some(el) = else_body {
            for expr in el {
                if let Expression::Return(_) = expr {
                    has_else_return = true;
                }
                else_return_value = self.translate_expression(expr);
            }
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

    fn translate_while_loop(&mut self, condition: Expression, loop_body: Vec<Expression>) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_ty =
            find_expression_type(&condition, &self.variables, self.functions, self.structs);
        if condition_ty.unwrap() != ast::Type::Bool {
            panic!("While loop condition must be a boolean expression");
        }

        let condition_value = self.translate_expression(condition);
        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        for expr in loop_body {
            self.translate_expression(expr);
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
    ) -> Value {
        let lhs_ty = find_expression_type(&lhs, &self.variables, self.functions, self.structs);
        let rhs_ty = find_expression_type(&rhs, &self.variables, self.functions, self.structs);

        if let Some(lt) = lhs_ty
            && let Some(rt) = rhs_ty
        {
            if lt != ast::Type::Bool || rt != ast::Type::Bool {
                panic!("Binary boolean expression must use boolean type");
            }
        } else {
            panic!("Binary boolean expressions type do not match");
        }

        let rhs = self.translate_expression(rhs);
        let lhs = self.translate_expression(lhs);

        match op {
            BooleanExpr::Or => self.builder.ins().bor(rhs, lhs),
            BooleanExpr::Xor => self.builder.ins().bxor(rhs, lhs),
            BooleanExpr::And => self.builder.ins().band(rhs, lhs),
            _ => unimplemented!(),
        }
    }

    fn translate_boolean_unary(&mut self, expr: Expression, op: BooleanExpr) -> Value {
        let ty = find_expression_type(&expr, &self.variables, self.functions, self.structs);

        if let Some(expr_ty) = ty {
            if expr_ty != ast::Type::Bool {
                panic!("Unary boolean expression must use boolean type");
            }
        }

        let expr = self.translate_expression(expr);

        match op {
            BooleanExpr::Not => self.builder.ins().bnot(expr),
            _ => unimplemented!(),
        }
    }
    fn translate_cmp(&mut self, comp: Comparison, lhs: Expression, rhs: Expression) -> Value {
        let ty_l =
            find_expression_type(&lhs, &self.variables, self.functions, self.structs).unwrap();
        let ty_r =
            find_expression_type(&rhs, &self.variables, self.functions, self.structs).unwrap();

        if ty_l != ty_r {
            panic!("Expression arms type do not match");
        }

        let cmp = {
            if ty_l.is_float() {
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
            Left(floatcc) => self.translate_fcmp(floatcc, lhs, rhs),
            Right(intcc) => self.translate_icmp(intcc, lhs, rhs),
        }
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expression, rhs: Expression) -> Value {
        let lhs = self.translate_expression(lhs);
        let rhs = self.translate_expression(rhs);
        self.builder.ins().icmp(cmp, lhs, rhs)
    }
    fn translate_fcmp(&mut self, cmp: FloatCC, lhs: Expression, rhs: Expression) -> Value {
        let lhs = self.translate_expression(lhs);
        let rhs = self.translate_expression(rhs);
        self.builder.ins().fcmp(cmp, lhs, rhs)
    }

    fn default_value(&mut self, ty: &ast::Type) -> Value {
        match ty {
            ast::Type::F32 => self.builder.ins().f32const(0.0),
            ast::Type::F64 => self.builder.ins().f64const(0.0),
            ast::Type::I8 => self.builder.ins().iconst(types::I8, 0),
            ast::Type::I16 => self.builder.ins().iconst(types::I16, 0),
            ast::Type::I32 => self.builder.ins().iconst(types::I32, 0),
            ast::Type::I64 => self.builder.ins().iconst(types::I64, 0),
            ast::Type::Bool => self.builder.ins().iconst(types::I8, 0),
            _ => unimplemented!(),
        }
    }
}

fn find_expression_type(
    expression: &Expression,
    variables: &HashMap<String, (Variable, ast::Type)>,
    functions: &Functions,
    structs: &Structs,
) -> Option<ast::Type> {
    // All literals are of same type
    let binary = |rhs: &Box<Expression>, lhs: &Box<Expression>| -> Option<ast::Type> {
        let left = find_expression_type(lhs, variables, functions, structs);
        let right = find_expression_type(rhs, variables, functions, structs);
        if let Some(l) = &left
            && let Some(r) = &right
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
        Expression::Identifier(ident) => Some(variables.get(ident).unwrap().1.clone()),
        Expression::Call(ref name, ref _exprs, _) => {
            functions.get(name).unwrap().return_type.clone()
        }
        Expression::Eq(_, _)
        | Expression::Ne(_, _)
        | Expression::Lt(_, _)
        | Expression::Le(_, _)
        | Expression::Gt(_, _)
        | Expression::Ge(_, _)
        | Expression::And(_, _)
        | Expression::Or(_, _)
        | Expression::Xor(_, _)
        | Expression::Not(_) => Some(ast::Type::Bool),
        Expression::Add(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Sub(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Mul(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Div(ref lhs, ref rhs) => binary(lhs, rhs),
        Expression::Assign(_, _) => None,
        Expression::Return(ref _expr) => None,
        Expression::StructInstantiate(ident, _) => Some(ast::Type::Struct(ident.clone())),
        Expression::StructMember(expr, member_ident) => {
            if let Some(ty) = find_expression_type(expr, variables, functions, structs) {
                if let ast::Type::Struct(s) = ty {
                    return Some(
                        structs
                            .get(&s)
                            .unwrap()
                            .fields
                            .get(member_ident)
                            .unwrap()
                            .clone(),
                    );
                }
            }
            return None;
        }
        Expression::Array(data) => {
            let types = data
                .iter()
                .map(|x| find_expression_type(x, variables, functions, structs))
                .collect::<Vec<_>>();
            let ty = types[0].clone().unwrap();
            let eq = types.iter().all(|x| x.clone().unwrap() == ty);
            if eq {
                Some(ast::Type::Array(Box::new(types[0].clone().unwrap())))
            } else {
                None
            }
        }
        _ => None,
    }
}
