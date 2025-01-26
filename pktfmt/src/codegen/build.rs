use std::io::Write;

use crate::ast::{max_value, DefaultVal, Header, LengthField};

use super::guard_assert_str;

/// A generator for various parse methods.
///
/// The goal of the parse is to convert a buffer type into a container type.
/// Currently, we consider two different buffer types, including the contiguous
/// and in-contiguous ones.
pub struct Build<'a> {
    header: &'a Header,
    length: &'a [LengthField],
}

impl<'a> Build<'a> {
    pub fn new(header: &'a Header, length: &'a [LengthField]) -> Self {
        Self { header, length }
    }

    pub fn code_gen_for_pktbuf(
        &self,
        method_name: &str,
        trait_type: &str,
        buf_name: &str,
        buf_type: &str,
        header_name: &str,
        header_type: &str,
        output: &mut dyn Write,
    ) {
        match (&self.length[1], &self.length[2]) {
            (LengthField::Undefined, _) | (_, LengthField::Undefined) => {
                // If either of the payload length or the packet length is
                // undefined, then we can not proceed generating the code.
                // This is because during the genneration process, we need to
                // set either the payload or the packet length in the
                // constructed packet.
                // If payload or packet length is not defined, then we will not be able to know
                // the exact field bit length used for calculating the payload or packet length.
                // As a result, we can not correctly apply packet format checking.
                return;
            }
            _ => {}
        }

        write!(
            output,
            "#[inline]
pub fn {method_name}<{trait_type}>(mut {buf_name}: {buf_type}, {header_name}: {header_type}) -> Self {{
"
        )
        .unwrap();

        let mut guards = Vec::new();
        let header_len_var = match &self.length[0] {
            LengthField::None => {
                // Fixed header length.
                guards.push(format!(
                    "{buf_name}.chunk_headeroom()>={}",
                    self.header.header_len_in_bytes()
                ));
                format!("{}", self.header.header_len_in_bytes())
            }
            LengthField::Undefined => {
                // Variable header length with undefined computing expression.
                write!(output, "let header_len = header.header_len() as usize;\n").unwrap();
                guards.push(format!("header_len>={}", self.header.header_len_in_bytes()));
                guards.push(format!("header_len<={buf_name}.chunk_headeroom()"));
                "header_len".to_string()
            }
            LengthField::Expr { expr } => {
                // Header field is defined with computing expression.
                write!(output, "let header_len = header.header_len() as usize;\n").unwrap();
                let (field, _) = self.header.field(expr.field_name()).unwrap();
                if field.default_fix {
                    // Variable header length with fixed default value.
                    let default_val = match field.default {
                        DefaultVal::Num(n) => n,
                        _ => panic!(),
                    };
                    let fixed_header_len = expr.exec(default_val).unwrap();
                    guards.push(format!("header_len=={fixed_header_len}"));
                } else {
                    // Variable header length.
                    guards.push(format!("header_len>={}", self.header.header_len_in_bytes()));
                }
                guards.push(format!("header_len<={buf_name}.chunk_headroom()"));
                "header_len".to_string()
            }
        };
        write!(output, "assert!({});\n", guard_assert_str(&guards, "&&")).unwrap();

        match (&self.length[1], &self.length[2]) {
            (LengthField::None, LengthField::None) => {
                // move the cursor back
                write!(output, "{buf_name}.move_back({header_len_var});\n",).unwrap();
                // copy the packet content in
                write!(
                    output,
                    "(&mut {buf_name}.chunk_mut()[0..{}]).copy_from_slice(header.as_bytes());\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();
                // If we do not have variable payload length or packet length,
                // we can construct a packet variable and return it
                write!(output, "Self {{ {buf_name} }}\n",).unwrap();
            }
            (LengthField::Expr { expr }, LengthField::None) => {
                write!(output, "let payload_len = {buf_name}.remaining();\n").unwrap();
                let (field, _) = self.header.field(expr.field_name()).unwrap();
                write!(
                    output,
                    "assert!(payload_len<={});\n",
                    max_value(field.bit).unwrap()
                )
                .unwrap();

                // move the cursor back
                write!(output, "{buf_name}.move_back({header_len_var});\n",).unwrap();
                // copy the packet content in
                write!(
                    output,
                    "(&mut {buf_name}.chunk_mut()[0..{}]).copy_from_slice(header.as_bytes());\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();

                // create a mutable packet variable
                write!(output, "let mut pkt = Self {{ {buf_name} }};\n",).unwrap();
                // setup the payload length
                write!(
                    output,
                    "pkt.set_payload_len(payload_len as {});\n",
                    field.repr.to_string()
                )
                .unwrap();
                write!(output, "pkt\n").unwrap();
            }
            (LengthField::None, LengthField::Expr { expr }) => {
                // move the cursor back
                write!(output, "{buf_name}.move_back({header_len_var});\n",).unwrap();

                write!(output, "let packet_len = {buf_name}.remaining();\n").unwrap();
                let (field, _) = self.header.field(expr.field_name()).unwrap();
                write!(
                    output,
                    "assert!(packet_len<={});\n",
                    max_value(field.bit).unwrap()
                )
                .unwrap();

                // copy the packet content in
                write!(
                    output,
                    "(&mut {buf_name}.chunk_mut()[0..{}]).copy_from_slice(header.as_bytes());\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();

                // create a mutable packet variable
                write!(output, "let mut pkt = Self {{ {buf_name} }};\n",).unwrap();
                // setup the packet length
                write!(
                    output,
                    "pkt.set_packet_len(packet_len as {});\n",
                    field.repr.to_string()
                )
                .unwrap();
                write!(output, "pkt\n").unwrap();
            }
            _ => {
                panic!()
            }
        }
        write!(output, "}}\n").unwrap();
    }
}
