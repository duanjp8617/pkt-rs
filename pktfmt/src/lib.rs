#[macro_use]
extern crate quick_error;

#[macro_use]
pub mod utils;

pub mod ast;
pub mod file_text;
pub mod token;
pub mod codegen;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);