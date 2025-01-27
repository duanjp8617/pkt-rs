use std::io::Write;

use crate::ast::{max_value, DefaultVal, Header, LengthField};

use super::{guard_assert_str, LengthSetMethod};

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
                    "(&mut {buf_name}.chunk_mut()[0..{}]).copy_from_slice(header.header_slice());\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();
                // If we do not have variable payload length or packet length,
                // we can construct a packet variable and return it
                write!(output, "Self {{ {buf_name} }}\n",).unwrap();
            }
            (LengthField::Expr { expr }, LengthField::None) => {
                write!(output, "let payload_len = {buf_name}.remaining();\n").unwrap();
                let (field, start) = self.header.field(expr.field_name()).unwrap();
                let payload_length_set = LengthSetMethod::new(field, start, expr);
                write!(
                    output,
                    "assert!(payload_len<={});\n",
                    payload_length_set.max_length(),
                )
                .unwrap();

                // move the cursor back
                write!(output, "{buf_name}.move_back({header_len_var});\n",).unwrap();
                // copy the packet content in
                write!(
                    output,
                    "(&mut {buf_name}.chunk_mut()[0..{}]).copy_from_slice(header.header_slice());\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();

                // create a mutable packet variable
                write!(output, "let mut container = Self {{ {buf_name} }};\n",).unwrap();
                // setup the payload length
                write!(
                    output,
                    "container.set_payload_len(payload_len as {});\n",
                    payload_length_set
                        .length_access_method_io_type()
                        .to_string()
                )
                .unwrap();
                write!(output, "container\n").unwrap();
            }
            (LengthField::None, LengthField::Expr { expr }) => {
                // move the cursor back
                write!(output, "{buf_name}.move_back({header_len_var});\n",).unwrap();

                write!(output, "let packet_len = {buf_name}.remaining();\n").unwrap();
                let (field, start) = self.header.field(expr.field_name()).unwrap();
                let packet_length_set = LengthSetMethod::new(field, start, expr);
                write!(
                    output,
                    "assert!(packet_len<={});\n",
                    packet_length_set.max_length(),
                )
                .unwrap();

                // copy the packet content in
                write!(
                    output,
                    "(&mut {buf_name}.chunk_mut()[0..{}]).copy_from_slice(header.header_slice());\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();

                // create a mutable packet variable
                write!(output, "let mut container = Self {{ {buf_name} }};\n",).unwrap();
                // setup the packet length
                write!(
                    output,
                    "container.set_packet_len(packet_len as {});\n",
                    packet_length_set.length_access_method_io_type().to_string()
                )
                .unwrap();
                write!(output, "container\n").unwrap();
            }
            _ => {
                panic!()
            }
        }
        write!(output, "}}\n").unwrap();
    }

    // impl<T: AsMut<[u8]>> Packet<T> {
    //     pub fn build_packet(buf: T) -> Self {
    //         assert!(buf.as_ref().len() >= 20);
    //         &mut buf.as_mut()[..20].copy_from_slice(fuck);
    //         Self{buf}
    //     }

    //     pub fn build_packet(mut buf: T, header_len: usize) -> Self {
    //         assert!(buf.as_mut().len() >= header_len && header_len <= max_value);
    //         &mut buf.as_mut()[..20].copy_from_slice(fuck);
    //         let container = Self {buf};
    //         container.set_header_len(header_len as yy)
    //         container
    //     }
    // }

    pub fn code_gen_for_contiguous_buffer(
        &self,
        method_name: &str,
        buf_name: &str,
        buf_type: &str,
        buf_access: &str,
        target_slice_name: &str,
        output: &mut dyn Write,
    ) {
        if matches!(self.length[0], LengthField::Undefined) {
            // We can't generate the method if the header length field is undefined.
            return;
        }

        match &self.length[0] {
            LengthField::None => {
                write!(
                    output,
                    "pub fn {method_name}({buf_name}: {buf_type}) -> Self {{\n"
                )
                .unwrap();
                write!(
                    output,
                    "assert!({buf_name}.{buf_access}.len() >= {});\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();
                write!(
                    output,
                    "&mut buf.{buf_access}[..{}].copy_from_slice({target_slice_name});\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();
                write!(output, "Self{{ {buf_name} }}\n").unwrap();
                write!(output, "}}\n").unwrap();
            }
            LengthField::Expr { expr } => {
                write!(
                    output,
                    "pub fn {method_name}({buf_name}: {buf_type}, header_len: usize) -> Self {{\n"
                )
                .unwrap();
                let (field, start) = self.header.field(expr.field_name()).unwrap();
                let header_length_set = LengthSetMethod::new(field, start, expr);
                write!(
                    output,
                    "assert!(({buf_name}.{buf_access}.len() >= header_len) && (header_len >= {}) && (header_len <= {}));\n",
                    self.header.header_len_in_bytes(),
                    header_length_set.max_length(),
                )
                .unwrap();
                write!(
                    output,
                    "&mut buf.{buf_access}[..{}].copy_from_slice({target_slice_name});\n",
                    self.header.header_len_in_bytes()
                )
                .unwrap();
                write!(output, "let mut container = Self{{ {buf_name} }};\n").unwrap();
                write!(
                    output,
                    "container.set_header_len(header_len as {});",
                    header_length_set.length_access_method_io_type().to_string()
                )
                .unwrap();
                write!(output, "container\n").unwrap();
                write!(output, "}}\n").unwrap();
            }
            _ => panic!(),
        }
    }
}
