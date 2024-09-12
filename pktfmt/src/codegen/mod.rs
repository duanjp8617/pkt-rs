use std::io::Write;

use crate::ast::{Header, Length, LengthField, Packet};

mod field;
pub use field::*;

mod length;
pub use length::*;

mod packet;
pub use packet::*;

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

#[cfg(test)]
mod tests {
    use crate::ast::BitPos;
    use crate::token::Tokenizer;

    use super::*;

    macro_rules! do_test_field_codegen {
        ($program: expr, $test_ty: ident, $test_fn: ident, $expected: expr, $start: expr $(, $arg: expr)*) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_ty::new(&field, $start).$test_fn($($arg,)* &mut buf);
            assert_eq!($expected, std::str::from_utf8(&buf[..]).unwrap());
        };
    }

    #[allow(unused_macros)]
    macro_rules! print_field_codegen {
        ($program: expr, $test_fn: ident, $expected: expr $(, $arg: expr)*) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_fn(&field $(, $arg)* , &mut buf);
            println!("{}", std::str::from_utf8(&buf[..]).unwrap())
        };
    }

    #[test]
    fn test_bracket_writer() {
        let mut s: Vec<u8> = ::std::vec::Vec::new();

        {
            let mut writer = HeadTailWriter::new(&mut s, "(", ")");
            write!(writer.get_writer(), "{}", 222).unwrap();
        }

        write!(&mut s, "{}", 555).unwrap();

        {
            let mut writer = HeadTailWriter::new(&mut s, "(", ")");
            write!(writer.get_writer(), "{}", 777).unwrap();
        }

        assert_eq!(std::str::from_utf8(&s[..]).unwrap(), "(222)555(777)");
    }

    #[test]
    fn test_read_repr_8b() {
        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldGetMethod,
            read_repr,
            "(self.buf.as_ref()[0]>>2)&0x1f",
            BitPos::new(0 * 8 + 1),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldGetMethod,
            read_repr,
            "self.buf.as_ref()[0]&0x1f",
            BitPos::new(0 * 8 + 3),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldGetMethod,
            read_repr,
            "self.buf.as_ref()[0]>>3",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldGetMethod,
            read_repr,
            "self.buf.as_ref()[0]",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 3}",
            FieldGetMethod,
            read_repr,
            "((self.buf.as_ref()[0]<<1)|(self.buf.as_ref()[1]>>7))&0x7",
            BitPos::new(0 * 8 + 6),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldGetMethod,
            read_repr,
            "(self.buf.as_ref()[0]<<6)|(self.buf.as_ref()[1]>>2)",
            BitPos::new(0 * 8 + 6),
            "self.buf.as_ref()"
        );
    }

    #[test]
    fn test_read_repr_gt_8b() {
        do_test_field_codegen!(
            "Field {bit  = 9}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])>>7",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 14}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])&0x3fff",
            BitPos::new(0 * 8 + 2),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 16}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 16, repr = &[u8]}",
            FieldGetMethod,
            read_repr,
            "&self.buf.as_ref()[0..2]",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 55}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_uint(&self.buf.as_ref()[3..10], 7)>>1",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 60}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u64(&self.buf.as_ref()[3..11])&0xfffffffffffffff",
            BitPos::new(3 * 8 + 4),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 128}",
            FieldGetMethod,
            read_repr,
            "&self.buf.as_ref()[3..19]",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_ref()"
        );
    }

    #[test]
    fn test_read_arg() {
        do_test_field_codegen!(
            "Field {bit  = 32, repr = &[u8], arg = %%Ipv4Addr%%, default=[0,0,0,0]}",
            FieldGetMethod,
            read_as_arg,
            "Ipv4Addr::from_byte_slice(&self.buf.as_ref()[3..7])",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit = 1, arg = bool, default = false}",
            FieldGetMethod,
            read_as_arg,
            "self.buf.as_ref()[13]&0x80 != 0",
            BitPos::new(13 * 8 + 0),
            "self.buf.as_ref()"
        );
    }

    #[test]
    fn test_write_repr_8b() {
        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0x83)|(value<<2);",
            BitPos::new(0 * 8 + 1),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0xe0)|value;",
            BitPos::new(0 * 8 + 3),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0x07)|(value<<3);",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=value;",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 3}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0xfc)|(value>>1);
self.buf.as_mut()[1]=(self.buf.as_mut()[1]&0x7f)|(value<<7);",
            BitPos::new(0 * 8 + 6),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0xfc)|(value>>6);
self.buf.as_mut()[1]=(self.buf.as_mut()[1]&0x03)|(value<<2);",
            BitPos::new(0 * 8 + 6),
            "self.buf.as_mut()",
            "value"
        );
    }

    #[test]
    fn test_write_repr_gt_8b() {
        do_test_field_codegen!(
            "Field {bit  = 9}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=(self.buf.as_mut()[1]&0x7f) as u16;
NetworkEndian::write_u16(&mut self.buf.as_mut()[0..2],rest_of_field|(value<<7));",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 14}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=((self.buf.as_mut()[0]&0xc0) as u16) << 8;
NetworkEndian::write_u16(&mut self.buf.as_mut()[0..2],rest_of_field|value);",
            BitPos::new(0 * 8 + 2),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 16}",
            FieldSetMethod,
            write_repr,
            "NetworkEndian::write_u16(&mut self.buf.as_mut()[0..2],value);",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 16, repr = &[u8]}",
            FieldSetMethod,
            write_repr,
            "(&mut self.buf.as_mut()[0..2]).copy_from_slice(value);",
            BitPos::new(0 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 55}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=(self.buf.as_mut()[9]&0x1) as u64;
NetworkEndian::write_uint(&mut self.buf.as_mut()[3..10],rest_of_field|(value<<1),7);",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 60}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=((self.buf.as_mut()[3]&0xf0) as u64) << 56;
NetworkEndian::write_u64(&mut self.buf.as_mut()[3..11],rest_of_field|value);",
            BitPos::new(3 * 8 + 4),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 128}",
            FieldSetMethod,
            write_repr,
            "(&mut self.buf.as_mut()[3..19]).copy_from_slice(value);",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );
    }

    #[test]
    fn test_write_arg() {
        do_test_field_codegen!(
            "Field {bit  = 32, repr = &[u8], arg = %%Ipv4Addr%%, default=[0,0,0,0]}",
            FieldSetMethod,
            write_as_arg,
            "let value = value.as_byte_slice();
(&mut self.buf.as_mut()[3..7]).copy_from_slice(value);",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit = 1, arg = bool, default = false}",
            FieldSetMethod,
            write_as_arg,
            "if value {
self.buf.as_mut()[13]=self.buf.as_mut()[13]|0x80
} else {
self.buf.as_mut()[13]=self.buf.as_mut()[13]&0x7f
}",
            BitPos::new(13 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 35}",
            FieldSetMethod,
            write_as_arg,
            "assert!(value <= 0x7ffffffff);
let rest_of_field=(self.buf.as_mut()[7]&0x1f) as u64;
NetworkEndian::write_uint(&mut self.buf.as_mut()[3..8],rest_of_field|(value<<5),5);",
            BitPos::new(3 * 8 + 0),
            "self.buf.as_mut()",
            "value"
        );
    }
}
