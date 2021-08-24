use crate::language::*;

pub struct ASTU {
  functions: Vec<FunctionU>,
}

#[derive(Debug)]
pub struct FunctionU {
  pub name: String,
  pub function_type: Type,
  pub function_body: Vec<InstructionU>
}

#[derive(Debug)]
pub enum InstructionU {
  Dup,
  Push(ValueU),
  Equals,
  ThenElse(Vec<InstructionU>, Vec<InstructionU>),
  Drop,
  Dive(usize),
  Add,
  Native(String),
  Subtract,
  Call(String)
}

#[derive(Debug)]
pub struct ValueU {
  pub value_type: Type,
  pub value: ValueUE
}

#[derive(Debug)]
pub enum ValueUE {
  Number(u64)
}