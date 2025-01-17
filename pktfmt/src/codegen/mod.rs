use std::io::Write;

use crate::ast::{Header, Length, LengthField, Packet};

mod field;
pub use field::*;

mod length;
pub use length::*;

mod packet;
pub use packet::*;

mod message;
pub use message::*;

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

// A trait that can help generate all the field access methods.
trait GenerateFieldAccessMethod {
    const LENGTH_FIELD_NAMES: &'static [&'static str] =
        &["header_len", "payload_len", "packet_len"];

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
impl_generate_field_access_method!(Packet);