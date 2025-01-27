use std::io::Write;

use super::HeadTailWriter;

/// A generator for a generic container type.
///
/// It generates the following type, which just wraps a generic variable named
/// `buf`:
///  pub struct name<T> {
///     buf: T
/// }
pub struct Container<'a> {
    pub container_struct_name: &'a str,
    pub derives: &'a [&'static str],
}

impl<'a> Container<'a> {
    // Generate the struct definition with derive attributes.
    pub fn code_gen(&self, mut output: &mut dyn Write) {
        assert!(self.derives.len() > 0);
        {
            let mut derive_writer = HeadTailWriter::new(&mut output, "#[derive(", ")]\n");
            self.derives
                .iter()
                .enumerate()
                .for_each(|(idx, derive_name)| {
                    write!(derive_writer.get_writer(), "{derive_name}").unwrap();
                    if idx < self.derives.len() - 1 {
                        write!(derive_writer.get_writer(), ",").unwrap();
                    }
                });
        }
        write!(
            output,
            "pub struct {}<T> {{
buf: T
}}
",
            self.container_struct_name
        )
        .unwrap();
    }

    // Wrap a `buf` inside a container.
    pub fn code_gen_for_parse_unchecked(buf_name: &str, buf_type: &str, output: &mut dyn Write) {
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

    // Return an imutable reference to the contained `buf`.
    pub fn code_gen_for_buf(buf_name: &str, buf_type: &str, output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn buf(&self) -> &{buf_type}{{
&self.{buf_name}
}}
"
        )
        .unwrap();
    }

    // Release the `buf` from the container.
    pub fn code_gen_for_release(buf_name: &str, buf_type: &str, output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn release(self) -> {buf_type}{{
self.{buf_name}
}}
"
        )
        .unwrap();
    }

    pub fn code_gen_for_header_slice(
        method_name: &str,
        mutable_op: &str,
        buf_access: &str,
        header_len: &str,
        output: &mut dyn Write,
    ) {
        write!(
            output,
            "#[inline]
pub fn {method_name}({mutable_op}self) -> {mutable_op}[u8]{{
{mutable_op}self{buf_access}[0..{header_len}]
}}
"
        )
        .unwrap();
    }

    // A generator for the option bytes.
    pub fn code_gen_for_option_slice(
        method_name: &str,
        mutable_op: &str,
        buf_access: &str,
        header_len: &str,
        output: &mut dyn Write,
    ) {
        write!(
            output,
            "#[inline]
pub fn {method_name}({mutable_op}self)->{mutable_op}[u8]{{
let header_len = (self.header_len() as usize);
{mutable_op}self{buf_access}[{header_len}..header_len]
}}
"
        )
        .unwrap();
    }
}
