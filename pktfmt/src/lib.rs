#[macro_use]
extern crate quick_error;

#[macro_use]
pub mod utils;

pub const COMPILER_MAX_NUM: u64 = 65535;
pub const USIZE_BYTES: u64 = 8;

pub mod ast;

// pub mod codegen;
pub mod file_text;
pub mod token;
pub mod codegen;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);
