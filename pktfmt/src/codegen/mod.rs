use std::io::Write;

use crate::ast::{Header, Length, LengthField, Message, Packet};

mod header;

mod packet;
pub use packet::*;

mod message;
pub use message::*;

mod container;

mod build;
mod field;
mod length;
mod parse;
mod payload;

use field::*;
use length::*;

// A writer object that appends prefix string and prepends suffix string to the
// underlying content.
struct HeadTailWriter<T: Write> {
    writer: T,
    tail: String,
}

impl<T: Write> HeadTailWriter<T> {
    fn new(mut writer: T, head: &str, tail: &str) -> Self {
        write!(writer, "{head}").unwrap();
        HeadTailWriter {
            writer,
            tail: tail.to_string(),
        }
    }

    fn get_writer(&mut self) -> &mut T {
        &mut self.writer
    }
}

impl<T: Write> Drop for HeadTailWriter<T> {
    fn drop(&mut self) {
        write!(&mut self.writer, "{}", self.tail).unwrap();
    }
}

// A helper struct for generating common struct definitino.
struct StructDefinition<'a> {
    struct_name: &'a str,
    derives: &'a [&'static str],
}

impl<'a> StructDefinition<'a> {
    // Generate the struct definition with derive attributes.
    fn code_gen(&self, mut output: &mut dyn Write) {
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
            self.struct_name
        )
        .unwrap();
    }

    // Wrap a `buf` inside a packet struct.
    fn parse_unchecked(output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn parse_unchecked(buf: T) -> Self{{
Self{{buf}}
}}
"
        )
        .unwrap();
    }

    // Return an imutable reference to the contained `buf`.
    fn buf(output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn buf(&self) -> &T{{
&self.buf
}}
"
        )
        .unwrap();
    }

    // Release the `buf` from the containing packet struct.
    fn release(output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn release(self) -> T{{
self.buf
}}
"
        )
        .unwrap();
    }
}

// Generate an implementation block for header/packet/message struct type.
fn impl_block<'out>(
    trait_name: &str,
    type_name: &str,
    type_param: &str,
    output: &'out mut dyn Write,
) -> HeadTailWriter<&'out mut dyn Write> {
    HeadTailWriter::new(
        output,
        &format!("impl<{trait_name}> {type_name}<{type_param}>{{\n"),
        "}\n",
    )
}

// A trait that can help generate all the field access methods.
trait GenerateFieldAccessMethod {
    const LENGTH_FIELD_NAMES: &'static [&'static str] =
        &["header_len", "payload_len", "packet_len"];

    fn protocol_name(&self) -> &str;
    fn header(&self) -> &Header;
    fn length(&self) -> &Length;

    fn header_field_method_gen(
        &self,
        target_slice: &str,
        write_value: Option<&str>,
        output: &mut dyn Write,
    ) {
        for (field_name, field, start) in self.header().field_iter() {
            match write_value {
                Some(write_value) => {
                    FieldSetMethod::new(field, start).code_gen(
                        field_name,
                        target_slice,
                        write_value,
                        output,
                    );
                }
                None => {
                    FieldGetMethod::new(field, start).code_gen(field_name, target_slice, output)
                }
            }
        }
    }

    fn length_field_method_gen(
        &self,
        target_slice: &str,
        write_value: Option<&str>,
        output: &mut dyn Write,
    ) {
        for index in 0..3 {
            match self.length().at(index) {
                LengthField::Expr { expr } => {
                    let (field, start) = self.header().field(expr.field_name()).unwrap();
                    match write_value {
                        Some(write_value) => {
                            LengthSetMethod::new(field, start, expr).code_gen(
                                Self::LENGTH_FIELD_NAMES[index],
                                target_slice,
                                write_value,
                                output,
                            );
                        }
                        None => {
                            LengthGetMethod::new(field, start, expr).code_gen(
                                Self::LENGTH_FIELD_NAMES[index],
                                target_slice,
                                output,
                            );
                        }
                    }
                }
                _ => {} // do nothing
            }
        }
    }
}

macro_rules! impl_generate_field_access_method {
    ($($ast_ty: ident),*) => {
        $(
            impl $crate::codegen::GenerateFieldAccessMethod for $ast_ty {
                fn protocol_name(&self) -> &str {
                    self.protocol_name()
                }

                fn header(&self) -> &$crate::ast::Header {
                    self.header()
                }

                fn length(&self) -> &$crate::ast::Length {
                    self.length()
                }
            }
        )*
    };
}
impl_generate_field_access_method!(Packet, Message);

fn guard_assert_str(guards: &Vec<String>, comp: &str) -> String {
    if guards.len() == 1 {
        format!("{}", guards[0])
    } else {
        let mut buf = Vec::new();
        guards.iter().enumerate().for_each(|(idx, s)| {
            write!(&mut buf, "({s})").unwrap();

            if idx < guards.len() - 1 {
                write!(&mut buf, "{comp}").unwrap();
            }
        });
        String::from_utf8(buf).unwrap()
    }
}
