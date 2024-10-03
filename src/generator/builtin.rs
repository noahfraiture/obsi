use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use std::collections::HashSet;

use super::CodeGen;
use super::GeneratorError;

#[derive(PartialEq, Eq, Hash)]
pub enum BuiltinFunction {
    Malloc,
}

pub type BuiltinTable = HashSet<BuiltinFunction>;

impl<'ctx> CodeGen<'ctx> {
    fn add_builtin(&self) {
        let arg_type = self.context.i64_type();
        let ret_type = self.context.ptr_type(AddressSpace::default());

        let malloc_type = ret_type.fn_type(&[arg_type.into()], false);
        self.module_builtin
            .add_function("malloc", malloc_type, None);
    }

    fn gen_malloc(&self, args: Vec<BasicMetadataValueEnum<'ctx>>) -> PointerValue<'ctx> {
        if !self.builtins.contains(&BuiltinFunction::Malloc) {
            self.add_builtin();
        }

        let malloc_fn = self
            .module_builtin
            .get_function("malloc")
            .expect("malloc not found after add_builtin");

        self.builder_builtin
            .build_call(malloc_fn, &args, "alloc_call")
            .expect("call malloc")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value()
    }

    pub fn gen_builtin(
        &self,
        name: &str,
        args: Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> Result<PointerValue<'ctx>, GeneratorError> {
        match name {
            "malloc" => {
                if args.len() == 1 && args[0].is_int_value() {
                    Ok(self.gen_malloc(args))
                } else {
                    Err(GeneratorError)
                }
            }
            _ => Err(GeneratorError),
        }
    }
}
