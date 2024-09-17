use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};

use crate::parser::ast::{BinOp, Expr, Literal, Program, Stmt};

pub struct CodeGenerator<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub target_machine: TargetMachine,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        // Initialize target system
        Target::initialize_all(&InitializationConfig::default());

        // Create module
        let module = context.create_module(module_name);

        // Create builder
        let builder = context.create_builder();

        // Create target machine
        let target = Target::from_triple(&TargetMachine::get_default_triple())
            .expect("Failed to create target from triple");
        let target_machine = target
            .create_target_machine(
                &TargetMachine::get_default_triple(),
                "generic",
                "",
                inkwell::OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Failed to create target machine");

        CodeGenerator {
            context,
            builder,
            module,
            target_machine,
        }
    }

    fn write_object(&self, object_path: &str) {
        self.target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(object_path))
            .expect("Failed to write object file");
    }

    fn link_object(&self, object_path: &str, bin_path: &str) {
        let mut command = std::process::Command::new("clang");
        command.args([object_path, "-o", bin_path]);
        command.status().expect("Failed to link object file");
    }

    fn run_bin(&self, bin_path: &str) {
        let mut command = std::process::Command::new(bin_path);
        println!("Status code :{:?}", command.status());
        command.status().expect("Failed to run binary");
    }

    pub fn gen(&self, program: &Program) {
        for statement in &program.0 {
            self.gen_stmt(statement)
        }
    }

    fn gen_stmt(&self, stmt: &Stmt) {
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

    fn gen_stmt_declare(&self, size: u8, name: &str) {
        todo!()
    }

    fn gen_stmt_assignment(&self, name: &str, value: &Expr) {
        todo!()
    }

    fn gen_stmt_expr(&self, value: &Expr) {
        todo!()
    }

    fn gen_stmt_function(&self, size: u8, name: &str, args: &[(u8, String)], body: &Stmt) {
        todo!()
    }

    fn gen_stmt_return(&self, value: &Expr) {
        todo!()
    }

    fn gen_stmt_block(&self, stmts: &[Stmt]) {
        todo!()
    }

    fn gen_stmt_if(&self, condition: &Expr, consequence: &Stmt, alternative: &Option<Box<Stmt>>) {
        todo!()
    }

    fn gen_expr(&self, expr: &Expr) {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Int(value) => self.gen_expr_int(*value),
                Literal::Float(value) => self.gen_expr_float(*value),
            },
            Expr::Variable(name) => self.gen_expr_variable(name),
            Expr::Infix(left, symbol, right) => self.gen_expr_infix(left, symbol, right),
            Expr::Not(value) => self.gen_expr_not(value),
            Expr::Call(function, args) => self.gen_expr_call(function, args),
        }
    }

    fn gen_expr_int(&self, value: i64) {
        todo!()
    }

    fn gen_expr_float(&self, value: f64) {
        todo!()
    }

    fn gen_expr_variable(&self, name: &str) {
        todo!()
    }

    fn gen_expr_infix(&self, left: &Expr, symbol: &BinOp, right: &Expr) {
        todo!()
    }

    fn gen_expr_not(&self, value: &Expr) {
        todo!()
    }

    fn gen_expr_call(&self, function: &Expr, args: &[Expr]) {
        todo!()
    }
}
