use std::io::Write;

use crate::ast::Length;

use super::{impl_block, GenerateFieldAccessMethod, StructDefinition};
use crate::ast::{DefaultVal, LengthField, Packet};

use super::HeadTailWriter;

/// A generator for various parse methods.
///
/// The goal of the parse is to convert a buffer type into a container type.
/// Currently, we consider two different buffer types, including the contiguous
/// and in-contiguous ones.
pub struct Parse<'a> {
    length: &'a Length,
    header_len_in_bytes: usize,
}

impl<'a> Parse<'a> {
    pub fn new(length: &'a Length, header_len_in_bytes: usize) -> Self {
        Self {
            length,
            header_len_in_bytes,
        }
    }

    /// Generator for `parse_unchecked`.
    /// Each container type will have a `parse_unchecked` method,  which just
    /// wraps the buffer type inside the container type.
    pub fn code_gen_for_parse_unchecked(
        &self,
        buf_name: &str,
        buf_type: &str,
        output: &mut dyn Write,
    ) {
        write!(
            output,
            "#[inline]
pub fn parse_unchecked({buf_name}: {buf_type}) -> Self{{
Self{{ {buf_name} }}
}}
"
        )
        .unwrap();
    }

    pub fn code_gen_for_contiguous_buffer(
        &self,
        method_name: &str,
        buf_name: &str,
        buf_type: &str,
        buf_access: &str,
        output: &mut dyn Write,
    ) {
        let remaining_len = &format!("{buf_name}.{buf_access}().len()");

        write!(
            output,
            "#[inline]
pub fn {method_name}({buf_name}: {buf_type}) -> Result<Self, {buf_type}> {{
let remaining_len = {remaining_len};
if remainig_len < {} {{
return Err(buf);
}}
let container = Self{{ {buf_name} }};
",
            self.header_len_in_bytes
        )
        .unwrap();

        // // Format checks are appended to the `guards`.
        // let mut guards = Vec::new();
        // let header_len_var = match self.length.at(0) {
        //     LengthField::None => {
        //         // Header field is not defined, use the fixed header length.
        //         &format!("{}", self.header_len_in_bytes)
        //     }
        //     LengthField::Undefined => {
        //         // Header field is defined without computing expression.
        //         // The return value of the user defined `header_len` method
        // must convert to         // `usize` safely.
        //         guards.push(format!("(packet.header_len() as usize)<{}",
        // self.header_len_in_bytes));         guards.push(format!("
        // (packet.header_len() as usize)>remaining_len"));
        //         "(packet.header_len() as usize)"
        //     }
        //     LengthField::Expr { expr } => {
        //         // Header field is defined with computing expression.
        //         let (field, _) =
        // self.packet().header().field(expr.field_name()).unwrap();
        //         if field.default_fix {
        //             // The default is fixed, check against the fixed value.
        //             let default_val = match field.default {
        //                 DefaultVal::Num(n) => n,
        //                 _ => panic!(),
        //             };
        //             let fixed_header_len = expr.exec(default_val).unwrap();
        //             guards.push(format!(
        //                 "(packet.header_len() as usize)!={fixed_header_len}"
        //             ));
        //         } else {
        //             // The default is not fixed, check with the fixed header
        // length.             guards.push(format!("(packet.header_len()
        // as usize)<{header_len_name}"));         }
        //         guards.push(format!("(packet.header_len() as
        // usize)>chunk_len"));         "(packet.header_len() as usize)"
        //     }
        // };

        // if self.packet().length().at(1).appear() {
        //     // Add check for the variable payload length.
        //     guards.push(format!(
        //         "(packet.payload_len() as
        // usize)+{header_len_var}>packet.buf.remaining()"     ));
        // } else if self.packet().length().at(2).appear() {
        //     // Add check for the variable packet length.
        //     guards.push(format!("(packet.packet_len() as
        // usize)<{header_len_var}"));     guards.push(format!(
        //         "(packet.packet_len() as usize)>packet.buf.remaining()"
        //     ));
        // } else {
        //     // Do nothing
        // }
    }
}
