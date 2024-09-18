use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::{builder::Builder, values::BasicValueEnum};
use inkwell::{IntPredicate, OptimizationLevel};

use crate::parser::ast::{BinOp, Expr, Literal, Program, Stmt};

mod symbol_table;

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
            Stmt::Declare(size, name) => self.gen_stmt_declare(*size, name),
            Stmt::Assignment(name, value) => self.gen_stmt_assignment(name, value),
            Stmt::Expression(expr) => self.gen_stmt_expr(expr),
            Stmt::Function(size, name, args, body) => {
                self.gen_stmt_function(*size, name, args, body)
            }
            Stmt::Return(value) => self.gen_stmt_return(value),
            Stmt::BlockStatement(stmts) => self.gen_stmt_block(stmts),
            Stmt::If(condition, consequence, alternative) => {
                self.gen_stmt_if(condition, consequence, alternative)
            }
        }
    }

    fn gen_stmt_declare(&mut self, size: u32, name: &str) {
        // Arg is in byte, we need to convert it to bit
        let width = self.context.custom_width_int_type(8 * size);
        let ptr = self
            .builder
            .build_alloca(width, name)
            .expect("declare alloca");
        self.builder
            .build_store(ptr, width.const_zero())
            .expect("build store");
        self.table.insert(name.to_string(), ptr, width.into());
    }

    fn gen_stmt_assignment(&self, name: &str, value: &Expr) {
        let variable = self.table.get(name).expect("assign variable not found");
        let value = self.gen_expr(value);

        if variable.r#type != value.get_type() {
            panic!(
                "Type mismatch: {:?} {:?}",
                value.get_type(),
                variable.r#type
            );
        }

        self.builder
            .build_store(variable.ptr, value)
            .expect("Failed to build store");

        println!("Assigned value to {}: {:?}", name, value);
    }

    fn gen_stmt_expr(&self, value: &Expr) {
        self.gen_expr(value);
    }

    fn gen_stmt_function(&mut self, size: u32, name: &str, args: &[(u32, String)], body: &Stmt) {
        let return_type = self.context.custom_width_int_type(size);

        let arg_types: Vec<_> = args
            .iter()
            .map(|(arg_size, _)| self.context.custom_width_int_type(*arg_size).into())
            .collect();
        let fn_type = return_type.fn_type(&arg_types, false);

        let function = self.module.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        for (i, (_, arg_name)) in args.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(arg.get_type(), arg_name)
                .expect("alloc");
            self.builder
                .build_store(alloca, arg)
                .expect("store argument");
        }

        self.gen_stmt(body);

        // TODO : remove if already a return statement, or force return none in the ast
        self.builder.build_return(None).expect("build return");
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

    fn gen_stmt_if(
        &mut self,
        condition: &Expr,
        consequence: &Stmt,
        alternative: &Option<Box<Stmt>>,
    ) {
        let condition_value = self.gen_expr(condition).into_int_value();
        let then_bb = self
            .context
            .append_basic_block(self.module.get_last_function().unwrap(), "then");
        let else_bb = self
            .context
            .append_basic_block(self.module.get_last_function().unwrap(), "else");
        let merge_bb = self
            .context
            .append_basic_block(self.module.get_last_function().unwrap(), "merge");

        self.builder
            .build_conditional_branch(condition_value, then_bb, else_bb)
            .expect("build condition");

        // Then block
        self.builder.position_at_end(then_bb);
        self.gen_stmt(consequence);
        self.builder
            .build_unconditional_branch(merge_bb)
            .expect("build uncondition");

        // Else block
        self.builder.position_at_end(else_bb);
        if let Some(alt) = alternative {
            self.gen_stmt(alt);
        }
        self.builder
            .build_unconditional_branch(merge_bb)
            .expect("build merge");

        // Merge block
        self.builder.position_at_end(merge_bb);
    }

    fn gen_expr(&self, expr: &Expr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Int(value) => self.gen_expr_int(*value),
                Literal::Float(value) => self.gen_expr_float(*value),
            },
            Expr::Variable(name) => self.gen_expr_variable(name),
            Expr::Infix(left, symbol, right) => self.gen_expr_infix(left, symbol, right),
            Expr::Not(value) => self.gen_expr_not(value).into(),
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

        match symbol {
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
        }
        .expect("operation infix")
        .into()
    }

    fn gen_expr_not(&self, value: &Expr) -> inkwell::values::IntValue<'ctx> {
        let value = self.gen_expr(value).into_int_value();
        self.builder.build_not(value, "nottmp").expect("not value")
    }

    fn gen_expr_call(
        &self,
        function: &Expr,
        args: &[Expr],
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        todo!()
    }
}

mod tests {
    use super::*;
    use crate::parser::ast::Program;

    #[allow(dead_code)]
    fn test_program(program: Program, output: Option<i64>) {
        let context = Context::create();
        let module_name = "test_module";
        let mut codegen = CodeGen::new(&context, module_name);
        codegen.gen(&program);

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
        test_program(program, Some(value));
    }

    #[test]
    fn test_plus() {
        let program = Program(vec![Stmt::Function(
            8,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(8, "a".to_string()),
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
        test_program(program, Some(5));
    }

    #[test]
    fn test_declare() {
        let program = Program(vec![Stmt::Function(
            8,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(8, "a".to_string()),
                Stmt::Assignment("a".to_string(), Expr::Literal(Literal::Int(69))),
                Stmt::Return(Expr::Variable("a".to_string())),
            ])),
        )]);
        test_program(program, Some(69));
    }

    #[test]
    fn test_assignment() {
        let program = Program(vec![Stmt::Function(
            0,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(8, "a".to_string()),
                Stmt::Assignment("a".to_string(), Expr::Literal(Literal::Int(69))),
            ])),
        )]);
        test_program(program, None);
    }

    #[test]
    fn test_if() {
        let program = Program(vec![Stmt::Function(
            0,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![Stmt::If(
                Expr::Literal(Literal::Int(1)),
                Box::new(Stmt::BlockStatement(vec![])),
                Some(Box::new(Stmt::BlockStatement(vec![]))),
            )])),
        )]);
        test_program(program, None);
    }

    #[test]
    fn test_full() {
        use super::*;
        use inkwell::context::Context;

        // Define the test program
        let program = Program(vec![Stmt::Function(
            8,
            "compute".to_string(),
            vec![(8, "a".to_string()), (8, "b".to_string())],
            Box::new(Stmt::BlockStatement(vec![
                Stmt::Declare(8, "five".to_string()),
                Stmt::Declare(8, "c".to_string()),
                Stmt::Declare(8, "d".to_string()),
                Stmt::Assignment("five".to_string(), Expr::Literal(Literal::Int(5))),
                Stmt::Assignment(
                    "c".to_string(),
                    Expr::Infix(
                        Box::new(Expr::Variable("a".to_string())),
                        BinOp::Add,
                        Box::new(Expr::Variable("b".to_string())),
                    ),
                ),
                Stmt::If(
                    Expr::Infix(
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
                    )])),
                    Some(Box::new(Stmt::BlockStatement(vec![Stmt::Assignment(
                        "d".to_string(),
                        Expr::Infix(
                            Box::new(Expr::Variable("a".to_string())),
                            BinOp::Sub,
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
            assert_eq!(result, 10);
        }
    }
}
