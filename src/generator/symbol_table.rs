use inkwell::{types::BasicTypeEnum, values::PointerValue};
use std::collections::HashMap;

pub struct Variable<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub r#type: BasicTypeEnum<'ctx>,
}

pub struct SymbolTable<'ctx> {
    pub scopes: Vec<HashMap<String, Variable<'ctx>>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, ptr: PointerValue<'ctx>, r#type: BasicTypeEnum<'ctx>) {
        if let Some(current_scope) = self.scopes.last_mut() {
            let variable = Variable { ptr, r#type };
            current_scope.insert(name, variable);
        }
    }

    pub fn get(&self, name: &str) -> Option<&Variable<'ctx>> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }
}
