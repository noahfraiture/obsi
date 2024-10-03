use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;

use crate::parser::ast::Program;

mod builtin;
mod generate;
mod symbol_table;

pub fn run(program: &Program) -> i64 {
    let context = Context::create();
    let module_name = "main";
    let mut codegen = CodeGen::new(&context, module_name);
    codegen.gen(program);

    let llvm_ir = codegen.module_main.print_to_string().to_string();
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
    builder_main: Builder<'ctx>,
    module_main: Module<'ctx>,
    builder_builtin: Builder<'ctx>,
    module_builtin: Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    symbols: symbol_table::SymbolTable<'ctx>,
    builtins: builtin::BuiltinTable,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module_main = context.create_module(module_name);
        let builder_main = context.create_builder();
        let module_builtin = context.create_module("builtin");
        let builder_builtin = context.create_builder();
        let execution_engine = module_main
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let symbols = symbol_table::SymbolTable::new();
        let builtins = builtin::BuiltinTable::new();

        CodeGen {
            context,
            builder_main,
            module_main,
            builder_builtin,
            module_builtin,
            execution_engine,
            symbols,
            builtins,
        }
    }

    pub fn run(&self) {
        unsafe {
            self.execution_engine
                .run_function(self.module_main.get_function("main").unwrap(), &[]);
        }
    }

    pub fn gen(&mut self, program: &Program) {
        for statement in &program.0 {
            self.gen_stmt(statement)
        }
    }
}

#[derive(Debug)]
pub struct GeneratorError;

mod tests {

    use super::*;
    use crate::parser::ast::*;

    #[allow(dead_code)]
    fn test_program(program: &Program, output: Option<i64>) {
        let context = Context::create();
        let module_name = "test_module";
        let mut codegen = CodeGen::new(&context, module_name);
        codegen.gen(program);

        let llvm_ir = codegen.module_main.print_to_string().to_string();
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
    fn test_free() {
        let program = Program(vec![Stmt::Function(
            0,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![Stmt::Expression(Expr::Call(
                Box::new(Expr::Variable("free".to_string())),
                vec![Expr::Call(
                    Box::new(Expr::Variable("malloc".to_string())),
                    vec![Expr::Literal(Literal::Int(8))],
                )],
            ))])),
        )]);
        test_program(&program, None);
    }

    #[test]
    fn test_malloc() {
        let program = Program(vec![Stmt::Function(
            0,
            "main".to_string(),
            vec![],
            Box::new(Stmt::BlockStatement(vec![Stmt::Expression(Expr::Call(
                Box::new(Expr::Variable("malloc".to_string())),
                vec![Expr::Literal(Literal::Int(8))],
            ))])),
        )]);
        test_program(&program, None);
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

        let llvm_ir = codegen.module_main.print_to_string().to_string();
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
