use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;

use std::collections::HashSet;

use super::CodeGen;
use super::GeneratorError;

#[derive(PartialEq, Eq, Hash)]
pub enum BuiltinFunction {}

pub type BuiltinTable = HashSet<BuiltinFunction>;

impl<'ctx> CodeGen<'ctx> {
    fn add_builtin(&self) {}

    fn gen_malloc(&self, args: Vec<BasicMetadataValueEnum<'ctx>>) -> PointerValue<'ctx> {
        let malloc = self
            .builder_main
            .build_array_malloc(
                self.context.custom_width_int_type(8),
                args[0].into_int_value(),
                "call malloc",
            )
            .expect("call malloc");
        println!("malloc value : {:?}", malloc);
        malloc
    }

    fn gen_free(&self, args: Vec<BasicMetadataValueEnum<'ctx>>) {
        let free = self
            .builder_main
            .build_free(args[0].into_pointer_value())
            .expect("call free");
        println!("free value : {:?}", free);
    }

    pub fn gen_builtin(
        &self,
        name: &str,
        args: Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, GeneratorError> {
        match name {
            "malloc" => {
                if args.len() == 1 && args[0].is_int_value() {
                    Ok(self.gen_malloc(args).into())
                } else {
                    Err(GeneratorError)
                }
            }
            "free" => {
                if args.len() == 1 && args[0].is_pointer_value() {
                    self.gen_free(args);
                    // NOTE : might be pretty stupid
                    Ok(self.context.i8_type().const_int(0, false).into())
                } else {
                    Err(GeneratorError)
                }
            }
            _ => Err(GeneratorError),
        }
    }
}