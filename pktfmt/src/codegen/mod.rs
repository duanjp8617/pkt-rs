use std::io::{Read, Write};

use crate::ast::{BuiltinTypes, Header, Length, LengthField, Packet};

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

// Append corresponding read method that honors the network endianess to the
// input `writer`. We use the `byteorder` crate just like `smoltcp` here.
fn network_endian_read<T: Write>(writer: T, bit_len: u64) -> HeadTailWriter<T> {
    let byte_len = byte_len(bit_len);
    match byte_len {
        2 => HeadTailWriter::new(writer, "NetworkEndian::read_u16(", ")"),
        3 => HeadTailWriter::new(writer, "NetworkEndian::read_u24(", ")"),
        4 => HeadTailWriter::new(writer, "NetworkEndian::read_u32(", ")"),
        5 | 6 | 7 => HeadTailWriter::new(
            writer,
            "NetworkEndian::read_uint(",
            &format!(", {byte_len})"),
        ),
        8 => HeadTailWriter::new(writer, "NetworkEndian::read_u64(", ")"),
        _ => panic!(),
    }
}

// Similar to `network_endian_read`, but it appends the write method.
fn network_endian_write<T: Write>(writer: T, bit_len: u64) -> HeadTailWriter<T> {
    let byte_len = byte_len(bit_len);
    match byte_len {
        2 => HeadTailWriter::new(writer, "NetworkEndian::write_u16(", ");"),
        3 => HeadTailWriter::new(writer, "NetworkEndian::write_u24(", ");"),
        4 => HeadTailWriter::new(writer, "NetworkEndian::write_u32(", ");"),
        5 | 6 | 7 => HeadTailWriter::new(
            writer,
            "NetworkEndian::write_uint(",
            &format!(",{byte_len});"),
        ),
        8 => HeadTailWriter::new(writer, "NetworkEndian::write_u64(", ");"),
        _ => panic!(),
    }
}

// A helper that converts length in bit to length in bytes
fn byte_len(bit_len: u64) -> u64 {
    if bit_len % 8 == 0 {
        bit_len / 8
    } else {
        bit_len / 8 + 1
    }
}

// Generate bit mask with all ones from `low`-th bit to the `high`-th bit.
fn ones_mask(mut low: u64, high: u64) -> String {
    assert!(low <= high && high < 64);

    let mut s = String::new();
    (0..low / 4).for_each(|_| {
        s.push('0');
    });

    while low / 4 < high / 4 {
        match low % 4 {
            0 => s.insert(0, 'f'),
            1 => s.insert(0, 'e'),
            2 => s.insert(0, 'c'),
            3 => s.insert(0, '8'),
            _ => panic!(),
        }
        low += 4 - low % 4
    }

    let mut res = 0;
    for offset in low % 4..high % 4 + 1 {
        res += 2_i32.pow(offset as u32);
    }

    format!("{:#x}", res) + &s
}

// Generate bit mask with all zeros from `low`-th bit to the `high`-th bit.
fn zeros_mask(mut low: u64, high: u64) -> String {
    assert!(low <= high && high < 64);

    let mut s = String::new();
    (0..low / 4).for_each(|_| {
        s.push('f');
    });

    while low / 4 < high / 4 {
        match low % 4 {
            0 => s.insert(0, '0'),
            1 => s.insert(0, '1'),
            2 => s.insert(0, '3'),
            3 => s.insert(0, '7'),
            _ => panic!(),
        }
        low += 4 - low % 4
    }

    let mut res = 0;
    for offset in low % 4..high % 4 + 1 {
        res += 2_i32.pow(offset as u32);
    }

    let mut s = format!("{:#x}", 15 - res) + &s;

    let repr_len = match byte_len(high + 1) {
        1 => 1,
        2 => 2,
        3 | 4 => 4,
        5 | 6 | 7 | 8 => 8,
        _ => panic!(),
    };

    (0..(repr_len * 2 - (s.len() - 2))).for_each(|_| s.insert(2, 'f'));
    s
}

// If the `arg` is a rust type, then the rust type must implement a convert
// method that turns the `repr`-typed value into a `arg`-typed one.
//
// Take `Ipv4Addr` as an example, it should implement:
// pub fn Ipv4Addr::from_byte_slice(value: &[u8]) -> Ipv4Addr {...}
fn to_rust_type(repr: BuiltinTypes, rust_type_code: &str) -> String {
    match repr {
        BuiltinTypes::U8 | BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
            format!("{rust_type_code}::from_{}", repr.to_string())
        }
        BuiltinTypes::ByteSlice => {
            format!("{rust_type_code}::from_byte_slice")
        }
        _ => panic!(),
    }
}

// If the `arg` is a rust type, then the rust type must implement a convert
// method that turns the `arg`-typed value into a `repr`-typed one.
//
// Take `EtherType` as an example, it should implement:
// pub fn EtherType::as_u16(&self) -> u16 {...}
fn rust_var_as_repr(var_name: &str, repr: BuiltinTypes) -> String {
    match repr {
        BuiltinTypes::U8 | BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
            format!("{var_name}.as_{}()", repr.to_string())
        }
        BuiltinTypes::ByteSlice => {
            format!("{var_name}.as_byte_slice()")
        }
        _ => panic!(),
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
                LengthField::Expr {
                    expr,
                    fixed_length_opt: _,
                } => {
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
    fn test_bit_mask() {
        fn to_num_back_to_hex_string(bit_mask: String) {
            assert_eq!(
                bit_mask,
                format!("{:#x}", u64::from_str_radix(&bit_mask[2..], 16).unwrap())
            );
        }

        to_num_back_to_hex_string(ones_mask(14, 33));
        to_num_back_to_hex_string(zeros_mask(14, 33));

        to_num_back_to_hex_string(ones_mask(7, 22));
        to_num_back_to_hex_string(zeros_mask(7, 22));

        to_num_back_to_hex_string(ones_mask(55, 55));
        to_num_back_to_hex_string(zeros_mask(55, 55));

        to_num_back_to_hex_string(ones_mask(55, 63));
        assert_eq!(&zeros_mask(55, 63), "0x007fffffffffffff");

        to_num_back_to_hex_string(ones_mask(44, 45));
        to_num_back_to_hex_string(zeros_mask(44, 45));

        to_num_back_to_hex_string(ones_mask(0, 63));
        assert_eq!(&zeros_mask(0, 63), "0x0000000000000000");
    }

    #[test]
    fn test_read_repr_8b() {
        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldGetMethod,
            read_repr,
            "(self.buf.as_ref()[0]>>2)&0x1f",
            BitPos {
                byte_pos: 0,
                bit_pos: 1,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldGetMethod,
            read_repr,
            "self.buf.as_ref()[0]&0x1f",
            BitPos {
                byte_pos: 0,
                bit_pos: 3,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldGetMethod,
            read_repr,
            "self.buf.as_ref()[0]>>3",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldGetMethod,
            read_repr,
            "self.buf.as_ref()[0]",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 3}",
            FieldGetMethod,
            read_repr,
            "((self.buf.as_ref()[0]<<1)|(self.buf.as_ref()[1]>>7))&0x7",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldGetMethod,
            read_repr,
            "(self.buf.as_ref()[0]<<6)|(self.buf.as_ref()[1]>>2)",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
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
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 14}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])&0x3fff",
            BitPos {
                byte_pos: 0,
                bit_pos: 2,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 16}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 16, repr = &[u8]}",
            FieldGetMethod,
            read_repr,
            "&self.buf.as_ref()[0..2]",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 55}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_uint(&self.buf.as_ref()[3..10], 7)>>1",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 60}",
            FieldGetMethod,
            read_repr,
            "NetworkEndian::read_u64(&self.buf.as_ref()[3..11])&0xfffffffffffffff",
            BitPos {
                byte_pos: 3,
                bit_pos: 4,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit  = 128}",
            FieldGetMethod,
            read_repr,
            "&self.buf.as_ref()[3..19]",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
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
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "self.buf.as_ref()"
        );

        do_test_field_codegen!(
            "Field {bit = 1, arg = bool, default = false}",
            FieldGetMethod,
            read_as_arg,
            "self.buf.as_ref()[13]&0x80 != 0",
            BitPos {
                byte_pos: 13,
                bit_pos: 0,
            },
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
            BitPos {
                byte_pos: 0,
                bit_pos: 1,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0xe0)|value;",
            BitPos {
                byte_pos: 0,
                bit_pos: 3,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 5}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0x07)|(value<<3);",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=value;",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 3}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0xfc)|(value>>1);
self.buf.as_mut()[1]=(self.buf.as_mut()[1]&0x7f)|(value<<7);",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 8}",
            FieldSetMethod,
            write_repr,
            "self.buf.as_mut()[0]=(self.buf.as_mut()[0]&0xfc)|(value>>6);
self.buf.as_mut()[1]=(self.buf.as_mut()[1]&0x03)|(value<<2);",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
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
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 14}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=((self.buf.as_mut()[0]&0xc0) as u16) << 8;
NetworkEndian::write_u16(&mut self.buf.as_mut()[0..2],rest_of_field|value);",
            BitPos {
                byte_pos: 0,
                bit_pos: 2,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 16}",
            FieldSetMethod,
            write_repr,
            "NetworkEndian::write_u16(&mut self.buf.as_mut()[0..2],value);",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 16, repr = &[u8]}",
            FieldSetMethod,
            write_repr,
            "(&mut self.buf.as_mut()[0..2]).copy_from_slice(value);",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 55}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=(self.buf.as_mut()[9]&0x1) as u64;
NetworkEndian::write_uint(&mut self.buf.as_mut()[3..10],rest_of_field|(value<<1),7);",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 60}",
            FieldSetMethod,
            write_repr,
            "let rest_of_field=((self.buf.as_mut()[3]&0xf0) as u64) << 56;
NetworkEndian::write_u64(&mut self.buf.as_mut()[3..11],rest_of_field|value);",
            BitPos {
                byte_pos: 3,
                bit_pos: 4,
            },
            "self.buf.as_mut()",
            "value"
        );

        do_test_field_codegen!(
            "Field {bit  = 128}",
            FieldSetMethod,
            write_repr,
            "(&mut self.buf.as_mut()[3..19]).copy_from_slice(value);",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
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
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
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
            BitPos {
                byte_pos: 13,
                bit_pos: 0,
            },
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
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "self.buf.as_mut()",
            "value"
        );
    }
}
