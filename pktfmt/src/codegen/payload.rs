use std::io::Write;

use crate::ast::{Header, LengthField};

/// A generator for various parse methods.
///
/// The goal of the parse is to convert a buffer type into a container type.
/// Currently, we consider two different buffer types, including the contiguous
/// and in-contiguous ones.
pub struct Payload<'a> {
    header: &'a Header,
    length: &'a [LengthField],
}

impl<'a> Payload<'a> {
    pub fn new(header: &'a Header, length: &'a [LengthField]) -> Self {
        Self { header, length }
    }

    pub fn code_gen_for_pktbuf(
        &self,
        method_name: &str,
        buf_name: &str,
        buf_type: &str,
        output: &mut dyn Write,
    ) {
        // Dump the start of the function body.
        let header_len_name = format!("{}", self.header.header_len_in_bytes());
        write!(
            output,
            "#[inline]
pub fn {method_name}(self)->{buf_type}{{
"
        )
        .unwrap();

        // If we have a variable payload or packet length, it is possible that the total
        // packet length will be smaller than the buffer size. In that case, we will
        // trim off the trailing bytes from the underlying buffer before we release the
        // payload.
        if self.length[1].appear() {
            // The protocol has variable payload length.
            let header_len_var = if self.length[0].appear() {
                "(self.header_len() as usize)"
            } else {
                &header_len_name
            };
            write!(
                output,
                "assert!({header_len_var}+self.payload_len() as usize<=self.buf.remaining());
let trim_size = self.buf.remaining()-({header_len_var}+self.payload_len() as usize);
"
            )
            .unwrap();
        } else if self.length[2].appear() {
            // The protocol has variable packet length.
            write!(
                output,
                "assert!((self.packet_len() as usize)<=self.buf.remaining());
let trim_size = self.buf.remaining()-self.packet_len() as usize;
"
            )
            .unwrap();
        } else {
            // Do nothing.
        }

        let header_len_var = if self.length[0].appear() {
            // Here, we have variable header length, so we must save the length
            // to a local variable before we release the buffer.
            write!(output, "let header_len = self.header_len() as usize;\n").unwrap();
            "header_len"
        } else {
            // The header has fixed length, we use the pre-defined constant.
            &header_len_name
        };

        // Release the internal buffer from the packet and then trim off the trailing
        // bytes if necessary.
        write!(output, "let mut buf = self.{buf_name};\n").unwrap();
        if self.length[1].appear() || self.length[2].appear() {
            write!(
                output,
                "if trim_size > 0 {{
buf.trim_off(trim_size);
}}
"
            )
            .unwrap();
        }

        // Advance the cursor beyond the header.
        write!(
            output,
            "buf.advance({header_len_var});
buf
}}
"
        )
        .unwrap();
    }

    pub fn code_gen_for_contiguous_buffer(
        &self,
        method_name: &str,
        mutable_op: &str,
        buf_name: &str,
        buf_type: &str,
        buf_access: &str,
        writer_fn: impl Fn(&mut dyn Write, &str),
        output: &mut dyn Write,
    ) {
        write!(
            output,
            "#[inline]
pub fn {method_name}({mutable_op}self)->{buf_type}{{
"
        )
        .unwrap();

        let start_index = if self.length[0].appear() {
            write!(output, "let header_len = self.header_len() as usize;\n").unwrap();
            format!("header_len")
        } else {
            format!("{}", self.header.header_len_in_bytes())
        };

        let end_index = if self.length[1].appear() {
            write!(output, "let payload_len = self.payload_len() as usize;\n").unwrap();
            format!("({start_index}+payload_len)")
        } else if self.length[2].appear() {
            write!(output, "let packet_len = self.packet_len() as usize;\n").unwrap();
            format!("packet_len")
        } else {
            "".to_string()
        };

        writer_fn(
            output,
            &format!("{mutable_op}self.{buf_name}.{buf_access}[{start_index}..{end_index}]"),
        );

        write!(output, "}}\n").unwrap();
    }
}
