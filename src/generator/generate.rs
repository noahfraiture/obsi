use inkwell::{AddressSpace, IntPredicate};

use crate::parser;
use crate::parser::ast::{BinOp, Expr, Literal, PreOp, Size, Stmt};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_stmt(&mut self, stmt: &Stmt) {
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
                let ptr = self
                    .builder_main
                    .build_alloca(t, name)
                    .expect("declare alloca");
                self.builder_main
                    .build_store(ptr, t.const_zero())
                    .expect("build store");
                self.symbols.insert(name.to_string(), ptr, t.into());
            }
            Size::Ptr => {
                let t = self.context.ptr_type(AddressSpace::default());
                let ptr = self
                    .builder_main
                    .build_alloca(t, name)
                    .expect("declare alloca");
                self.builder_main
                    .build_store(ptr, t.const_zero())
                    .expect("build store");
                self.symbols.insert(name.to_string(), ptr, t.into());
            }
        };
    }

    fn gen_stmt_assignment(&self, name: &str, value: &Expr) {
        let variable = self.symbols.get(name).expect("assign variable not found");
        let value = self.gen_expr(value);

        if variable.r#type != value.get_type() {
            panic!(
                "Type mismatch:\n{:#?}\n{:#?}",
                value.get_type(),
                variable.r#type
            );
        }

        self.builder_main
            .build_store(variable.ptr, value)
            .expect("Failed to build store");
    }

    fn gen_stmt_expr(&self, value: &Expr) {
        self.gen_expr(value);
    }

    fn gen_stmt_func(&mut self, size: u32, name: &str, args: &[(u32, String)], body: &Stmt) {
        self.symbols.enter_scope();
        let return_type = self.context.custom_width_int_type(size);

        let arg_types: Vec<_> = args
            .iter()
            .map(|(arg_size, _)| self.context.custom_width_int_type(*arg_size * 8).into())
            .collect();
        let fn_type = return_type.fn_type(&arg_types, false);

        let function = self.module_main.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder_main.position_at_end(entry);

        for (i, (_, arg_name)) in args.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            let ptr = self
                .builder_main
                .build_alloca(arg.get_type(), arg_name)
                .expect("alloc");
            self.builder_main
                .build_store(ptr, arg)
                .expect("store argument");
            self.symbols
                .insert(arg_name.to_string(), ptr, arg.get_type());
        }

        self.gen_stmt(body);

        if !parser::has_return(body) {
            self.builder_main.build_return(None).expect("build return");
        }
        self.symbols.exit_scope();
    }

    fn gen_stmt_return(&self, value: &Expr) {
        let return_value = self.gen_expr(value).into_int_value();
        self.builder_main
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
            .builder_main
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

        self.builder_main
            .build_conditional_branch(condition_value, then_block, else_block)
            .expect("Failed to build conditional branch");

        // Generate then
        self.builder_main.position_at_end(then_block);
        self.gen_stmt(then_stmt);

        // Jump to merge if there is no terminator to the function (return)
        if self
            .builder_main
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder_main
                .build_unconditional_branch(merge_block)
                .expect("Failed to build unconditional branch to merge");
        }

        // Generate else
        self.builder_main.position_at_end(else_block);
        if let Some(alt_stmt) = else_stmt {
            self.gen_stmt(alt_stmt);
        }

        if self
            .builder_main
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder_main
                .build_unconditional_branch(merge_block)
                .expect("Failed to build unconditional branch to merge");
        }

        // Move to 'merge' block
        self.builder_main.position_at_end(merge_block);
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
            .symbols
            .get(name)
            .unwrap_or_else(|| panic!("expr variable not found {}", name));
        let loaded_value = self
            .builder_main
            .build_load(variable.r#type, variable.ptr, name);
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
            BinOp::Add => self.builder_main.build_int_add(left, right, "addtmp"),
            BinOp::Sub => self.builder_main.build_int_sub(left, right, "subtmp"),
            BinOp::Mul => self.builder_main.build_int_mul(left, right, "multmp"),
            BinOp::Div => self
                .builder_main
                .build_int_signed_div(left, right, "divtmp"),
            BinOp::Mod => self
                .builder_main
                .build_int_signed_rem(left, right, "modtmp"),
            BinOp::Less => {
                self.builder_main
                    .build_int_compare(IntPredicate::ULT, left, right, "lesstmp")
            }
            BinOp::Equal => {
                self.builder_main
                    .build_int_compare(IntPredicate::EQ, left, right, "eqtmp")
            }
            BinOp::Xor => self.builder_main.build_xor(left, right, "xortmp"),
            BinOp::Or => self.builder_main.build_or(left, right, "ortmp"),
            BinOp::And => self.builder_main.build_and(left, right, "andtmp"),
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
                self.builder_main
                    .build_ptr_to_int(expr, self.context.i64_type(), "dereftmp")
                    .expect("dereftmp")
                    .into()
            }
            // NOTE: compile at least
            PreOp::Ref => {
                let expr = self.gen_expr(expr).into_int_value();
                self.builder_main
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
                self.builder_main
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
        let name = match function_expr {
            Expr::Variable(name) => name,
            _ => todo!(), // value is pointer
        };
        let pointer = self.module_main.get_function(name);

        let compiled_args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
            args.iter().map(|arg| self.gen_expr(arg).into()).collect();

        if let Some(pointer) = pointer {
            self.builder_main
                .build_call(pointer, &compiled_args, "calltmp")
                .expect("call")
                .try_as_basic_value()
                .left()
                .unwrap()
        } else {
            self.gen_builtin(name, compiled_args)
                .expect("function not found")
                .into()
        }
    }
}
