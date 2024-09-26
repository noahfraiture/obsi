use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

use crate::parser;
use crate::parser::ast::{BinOp, Expr, Literal, PreOp, Program, Size, Stmt};

mod symbol_table;

pub fn run(program: &Program) -> i64 {
    let context = Context::create();
    let module_name = "main";
    let mut codegen = CodeGen::new(&context, module_name);
    codegen.gen(program);

    let llvm_ir = codegen.module.print_to_string().to_string();
    println!("{}", llvm_ir);

    let main_fn = unsafe {
        codegen
            .execution_engine
            .get_function::<unsafe extern "C" fn() -> i64>("main")
            .expect("Failed to get function")
    };

    let result;
    unsafe {
        result = main_fn.call();
    }
    result
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    table: symbol_table::SymbolTable<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let table = symbol_table::SymbolTable::new();

        CodeGen {
            context,
            builder,
            module,
            execution_engine,
            table,
        }
    }

    pub fn run(&self) {
        unsafe {
            self.execution_engine
                .run_function(self.module.get_function("main").unwrap(), &[]);
        }
    }

    pub fn gen(&mut self, program: &Program) {
        for statement in &program.0 {
            self.gen_stmt(statement)
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Declare(size, name) => self.gen_stmt_declare(size, name),
            Stmt::Assignment(name, value) => self.gen_stmt_assignment(name, value),
            Stmt::Expression(expr) => self.gen_stmt_expr(expr),
            Stmt::Function(size, name, args, body) => self.gen_stmt_func(*size, name, args, body),
            Stmt::Return(value) => self.gen_stmt_return(value),
            Stmt::BlockStatement(stmts) => self.gen_stmt_block(stmts),
            Stmt::If(condition, consequence, alternative) => {
                self.gen_stmt_if(condition, consequence, alternative)
            }
        }
    }

    fn gen_stmt_declare(&mut self, size: &Size, name: &str) {
        // TODO : refactor that shit
        match size {
            Size::Int(s) => {
                let t = self.context.custom_width_int_type(8 * s);
                let ptr = self.builder.build_alloca(t, name).expect("declare alloca");
                self.builder
                    .build_store(ptr, t.const_zero())
                    .expect("build store");
                self.table.insert(name.to_string(), ptr, t.into());
            }
            Size::Ptr => {
                let t = self.context.ptr_type(AddressSpace::default());
                let ptr = self.builder.build_alloca(t, name).expect("declare alloca");
                self.builder
                    .build_store(ptr, t.const_zero())
                    .expect("build store");
                self.table.insert(name.to_string(), ptr, t.into());
            }
        };
    }

    fn gen_stmt_assignment(&self, name: &str, value: &Expr) {
        let variable = self.table.get(name).expect("assign variable not found");
        let value = self.gen_expr(value);

        if variable.r#type != value.get_type() {
            panic!(
                "Type mismatch:\n{:#?}\n{:#?}",
                value.get_type(),
                variable.r#type
            );
        }

        self.builder
            .build_store(variable.ptr, value)
            .expect("Failed to build store");
    }

    fn gen_stmt_expr(&self, value: &Expr) {
        self.gen_expr(value);
    }

    fn gen_stmt_func(&mut self, size: u32, name: &str, args: &[(u32, String)], body: &Stmt) {
        self.table.enter_scope();
        let return_type = self.context.custom_width_int_type(size);

        let arg_types: Vec<_> = args
            .iter()
            .map(|(arg_size, _)| self.context.custom_width_int_type(*arg_size * 8).into())
            .collect();
        let fn_type = return_type.fn_type(&arg_types, false);

        let function = self.module.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        for (i, (_, arg_name)) in args.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            let ptr = self
                .builder
                .build_alloca(arg.get_type(), arg_name)
                .expect("alloc");
            self.builder.build_store(ptr, arg).expect("store argument");
            self.table.insert(arg_name.to_string(), ptr, arg.get_type());
        }

        self.gen_stmt(body);

        if !parser::has_return(body) {
            self.builder.build_return(None).expect("build return");
        }
        self.table.exit_scope();
    }

    fn gen_stmt_return(&self, value: &Expr) {
        let return_value = self.gen_expr(value).into_int_value();
        self.builder
            .build_return(Some(&return_value))
            .expect("build return");
    }

    fn gen_stmt_block(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.gen_stmt(stmt);
        }
    }

    // TODO : what is merge
    fn gen_stmt_if(&mut self, condition: &Expr, then_stmt: &Stmt, else_stmt: &Option<Box<Stmt>>) {
        let current_function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let condition_value = self.gen_expr(condition).into_int_value();

        // Create basic blocks for 'then', 'else', and 'merge'
        let then_block = self.context.append_basic_block(current_function, "then");
        let else_block = self.context.append_basic_block(current_function, "else");
        // block after if-else
        let merge_block = self.context.append_basic_block(current_function, "merge");

        self.builder
            .build_conditional_branch(condition_value, then_block, else_block)
            .expect("Failed to build conditional branch");

        // Generate then
        self.builder.position_at_end(then_block);
        self.gen_stmt(then_stmt);

        // Jump to merge if there is no terminator to the function (return)
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder
                .build_unconditional_branch(merge_block)
                .expect("Failed to build unconditional branch to merge");
        }

        // Generate else
        self.builder.position_at_end(else_block);
        if let Some(alt_stmt) = else_stmt {
            self.gen_stmt(alt_stmt);
        }

        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder
                .build_unconditional_branch(merge_block)
                .expect("Failed to build unconditional branch to merge");
        }

        // Move to 'merge' block
        self.builder.position_at_end(merge_block);
    }

    fn gen_expr(&self, expr: &Expr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Int(value) => self.gen_expr_int(*value),
                Literal::Float(value) => self.gen_expr_float(*value),
            },
            Expr::Variable(name) => self.gen_expr_variable(name),
            Expr::Infix(left, symbol, right) => self.gen_expr_infix(left, symbol, right),
            Expr::Prefix(symbol, right) => self.gen_expr_prefix(symbol, right),
            Expr::Call(function, args) => self.gen_expr_call(function, args),
        }
    }

    fn gen_expr_int(&self, value: i64) -> inkwell::values::BasicValueEnum<'ctx> {
        let int_value = self.context.i64_type().const_int(value as u64, false);
        int_value.into()
    }

    fn gen_expr_float(&self, value: f64) -> inkwell::values::BasicValueEnum<'ctx> {
        let float_value = self.context.f64_type().const_float(value);
        float_value.into()
    }

    fn gen_expr_variable(&self, name: &str) -> inkwell::values::BasicValueEnum<'ctx> {
        let variable = self
            .table
            .get(name)
            .unwrap_or_else(|| panic!("expr variable not found {}", name));
        let loaded_value = self.builder.build_load(variable.r#type, variable.ptr, name);
        loaded_value.expect("Variable load value")
    }

    fn gen_expr_infix(
        &self,
        left: &Expr,
        symbol: &BinOp,
        right: &Expr,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let left = self.gen_expr(left).into_int_value();
        let right = self.gen_expr(right).into_int_value();

        let value = match symbol {
            BinOp::Add => self.builder.build_int_add(left, right, "addtmp"),
            BinOp::Sub => self.builder.build_int_sub(left, right, "subtmp"),
            BinOp::Mul => self.builder.build_int_mul(left, right, "multmp"),
            BinOp::Div => self.builder.build_int_signed_div(left, right, "divtmp"),
            BinOp::Mod => self.builder.build_int_signed_rem(left, right, "modtmp"),
            BinOp::Less => {
                self.builder
                    .build_int_compare(IntPredicate::ULT, left, right, "lesstmp")
            }
            BinOp::Equal => self
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "eqtmp"),
            BinOp::Xor => self.builder.build_xor(left, right, "xortmp"),
            BinOp::Or => self.builder.build_or(left, right, "ortmp"),
            BinOp::And => self.builder.build_and(left, right, "andtmp"),
        };

        value.expect("operation infix").into()
    }

    fn gen_expr_prefix(
        &self,
        symbol: &PreOp,
        expr: &Expr,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        match symbol {
            PreOp::Deref => {
                let expr = self.gen_expr(expr).into_pointer_value();
                self.builder
                    .build_ptr_to_int(expr, self.context.i64_type(), "dereftmp")
                    .expect("dereftmp")
                    .into()
            }
            // NOTE: compile at least
            PreOp::Ref => {
                let expr = self.gen_expr(expr).into_int_value();
                self.builder
                    .build_int_to_ptr(
                        expr,
                        self.context.ptr_type(AddressSpace::default()),
                        "reftmp",
                    )
                    .expect("ref")
                    .into()
            }
            PreOp::Not => {
                let expr = self.gen_expr(expr).into_int_value();
                self.builder
                    .build_not(expr, "nottmp")
                    .expect("not value")
                    .into()
            }
        }
    }

    fn gen_expr_call(
        &self,
        function_expr: &Expr,
        args: &[Expr],
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let pointer = match function_expr {
            Expr::Variable(name) => self.module.get_function(name).expect("function not found"),
            _ => todo!(), // value is pointer
        };

        let compiled_args: Vec<inkwell::values::BasicMetadataValueEnum> =
            args.iter().map(|arg| self.gen_expr(arg).into()).collect();

        let call = self
            .builder
            .build_call(pointer, &compiled_args, "calltmp")
            .expect("call");

        // TODO : what ?
        call.try_as_basic_value().left().unwrap()
    }
}

mod tests {

    use super::*;
    use crate::parser::ast::Program;

    #[allow(dead_code)]
    fn test_program(program: &Program, output: Option<i64>) {
        let context = Context::create();
        let module_name = "test_module";
        let mut codegen = CodeGen::new(&context, module_name);
        codegen.gen(program);

        let llvm_ir = codegen.module.print_to_string().to_string();
        println!("{}", llvm_ir);

        if let Some(output) = output {
            let main = unsafe {
                codegen
                    .execution_engine
                    .get_function::<unsafe extern "C" fn() -> i64>("main")
                    .expect("Failed to get function")
            };
            unsafe {
                let result = main.call();
                assert_eq!(result, output);
            }
        } else {
            let main = unsafe {
                codegen
                    .execution_engine
                    .get_function::<unsafe extern "C" fn()>("main")
                    .expect("Failed to get function")
            };
            unsafe {
                main.call();
            }
        }
    }

    #[test]
    fn test_return() {
        let value = 69;
        let program = Program(vec![Stmt::Function(
            8,
            "main".to_string(),
            vec![],
            Box::new(Stmt::Return(Expr::Literal(Literal::Int(value)))),
        )]);
        test_program(&program, Some(value));
    }

    #[test]
    fn test_plus() {
        let program = Program(vec![Stmt::Function(
            8,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(8), "a".to_string()),
                Stmt::Assignment(
                    "a".to_string(),
                    Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(2))),
                        BinOp::Add,
                        Box::new(Expr::Literal(Literal::Int(3))),
                    ),
                ),
                Stmt::Return(Expr::Variable("a".to_string())),
            ])),
        )]);
        test_program(&program, Some(5));
    }

    #[test]
    fn test_declare() {
        let program = Program(vec![Stmt::Function(
            8,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(8), "a".to_string()),
                Stmt::Assignment("a".to_string(), Expr::Literal(Literal::Int(69))),
                Stmt::Return(Expr::Variable("a".to_string())),
            ])),
        )]);
        test_program(&program, Some(69));
    }

    #[test]
    fn test_assignment() {
        let program = Program(vec![Stmt::Function(
            0,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(8), "a".to_string()),
                Stmt::Assignment("a".to_string(), Expr::Literal(Literal::Int(69))),
            ])),
        )]);
        test_program(&program, None);
    }

    #[test]
    fn test_ref() {
        let program = Program(vec![Stmt::Function(
            8,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(8), "one".to_string()),
                Stmt::Assignment("one".to_string(), Expr::Literal(Literal::Int(1))),
                Stmt::Declare(Size::Ptr, "ptr".to_string()),
                Stmt::Assignment(
                    "ptr".to_string(),
                    Expr::Prefix(PreOp::Ref, Box::new(Expr::Variable("one".to_string()))),
                ),
                Stmt::Declare(Size::Int(8), "val".to_string()),
                Stmt::Assignment(
                    "val".to_string(),
                    Expr::Prefix(PreOp::Deref, Box::new(Expr::Variable("ptr".to_string()))),
                ),
                Stmt::Return(Expr::Variable("val".to_string())),
            ])),
        )]);
        test_program(&program, Some(1));
    }

    #[test]
    fn test_if() {
        let program = Program(vec![Stmt::Function(
            0,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(8), "x".to_string()),
                Stmt::If(
                    Expr::Literal(Literal::Int(1)),
                    Box::new(Stmt::Assignment(
                        "x".to_string(),
                        Expr::Literal(Literal::Int(1)),
                    )),
                    Some(Box::new(Stmt::Assignment(
                        "x".to_string(),
                        Expr::Literal(Literal::Int(0)),
                    ))),
                ),
                Stmt::If(
                    Expr::Literal(Literal::Int(0)),
                    Box::new(Stmt::Return(Expr::Literal(Literal::Int(0)))),
                    None,
                ),
                Stmt::Return(Expr::Variable("x".to_string())),
            ])),
        )]);
        test_program(&program, Some(1));
    }

    #[test]
    fn test_call() {
        let program = Program(vec![
            Stmt::Function(
                8,
                "add".to_string(),
                vec![(8, "a".to_string()), (8, "b".to_string())],
                Box::new(Stmt::BlockStatement(vec![Stmt::Return(Expr::Infix(
                    Box::new(Expr::Variable("a".to_string())),
                    BinOp::Add,
                    Box::new(Expr::Variable("b".to_string())),
                ))])),
            ),
            Stmt::Function(
                8,
                "main".to_string(),
                vec![],
                Box::new(Stmt::Return(Expr::Call(
                    Box::new(Expr::Variable("add".to_string())),
                    vec![
                        Expr::Literal(Literal::Int(1)),
                        Expr::Literal(Literal::Int(2)),
                    ],
                ))),
            ),
        ]);
        test_program(&program, Some(3));
    }

    #[test]
    fn test_full() {
        use super::*;
        use inkwell::context::Context;

        // Define the test program
        let program = Program(vec![Stmt::Function(
            8,
            "compute".to_string(),
            vec![(8, "a".to_string()), (8, "b".to_string())], // 2 and 3
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(Size::Int(8), "five".to_string()),
                Stmt::Declare(Size::Int(8), "c".to_string()),
                Stmt::Declare(Size::Int(8), "d".to_string()),
                Stmt::Assignment("five".to_string(), Expr::Literal(Literal::Int(5))),
                Stmt::Assignment(
                    "c".to_string(), // 5
                    Expr::Infix(
                        Box::new(Expr::Variable("a".to_string())),
                        BinOp::Add,
                        Box::new(Expr::Variable("b".to_string())),
                    ),
                ),
                Stmt::If(
                    Expr::Infix(
                        // true
                        Box::new(Expr::Variable("b".to_string())),
                        BinOp::Less,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    ),
                    Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::Infix(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Add,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    )])), // ignored
                    Some(Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::Infix(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Mul,
                            Box::new(Expr::Literal(Literal::Int(5))),
                        ),
                    )]))),
                ),
                Stmt::Return(Expr::Variable("d".to_string())),
            ])),
        )]);

        // Create a new LLVM context and module
        let context = Context::create();
        let module_name = "test_module";
        let mut codegen = CodeGen::new(&context, module_name);
        codegen.gen(&program);

        let llvm_ir = codegen.module.print_to_string().to_string();
        println!("{}", llvm_ir);

        let compute_fn = unsafe {
            codegen
                .execution_engine
                .get_function::<unsafe extern "C" fn(i64, i64) -> i64>("compute")
                .expect("Failed to get function")
        };

        unsafe {
            let result = compute_fn.call(2, 3);
            assert_eq!(result, 7);
        }
    }
}
