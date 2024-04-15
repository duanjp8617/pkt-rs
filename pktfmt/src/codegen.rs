use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::ast::*;

struct HeadTailWriter<T: Write> {
    writer: T,
    tail: String,
}

impl<T: Write> HeadTailWriter<T> {
    fn new(mut writer: T, head: &str, tail: &str) -> Self {
        write!(writer, "{}", head).unwrap();
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

fn network_endian_read<T: Write>(writer: T, bit_len: u64) -> HeadTailWriter<T> {
    let byte_len = byte_len(bit_len);
    match byte_len {
        2 => HeadTailWriter::new(writer, "NetworkEndian::read_u16(", ")"),
        3 => HeadTailWriter::new(writer, "NetworkEndian::read_u24(", ")"),
        4 => HeadTailWriter::new(writer, "NetworkEndian::read_u32(", ")"),
        5 | 6 | 7 => HeadTailWriter::new(
            writer,
            "NetworkEndian::read_uint(",
            &format!(", {})", byte_len),
        ),
        8 => HeadTailWriter::new(writer, "NetworkEndian::read_u64(", ")"),
        _ => panic!(),
    }
}

fn network_endian_write<T: Write>(writer: T, bit_len: u64) -> HeadTailWriter<T> {
    match byte_len(bit_len) {
        2 => HeadTailWriter::new(writer, "NetworkEndian::read_u16(", ")"),
        3 => HeadTailWriter::new(writer, "NetworkEndian::read_u24(", ")"),
        4 => HeadTailWriter::new(writer, "NetworkEndian::read_u32(", ")"),
        5 => HeadTailWriter::new(writer, "NetworkEndian::read_uint(", ", 5)"),
        6 => HeadTailWriter::new(writer, "NetworkEndian::read_uint(", ", 6)"),
        7 => HeadTailWriter::new(writer, "NetworkEndian::read_uint(", ", 7)"),
        8 => HeadTailWriter::new(writer, "NetworkEndian::read_u64(", ")"),
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

fn read_field(field: &Field, start: BitPos, target_slice: &str, mut output: &mut dyn Write) {
    match field.repr {
        BuiltinTypes::ByteSlice => {
            write!(
                output,
                "&{}[{}..{}]",
                target_slice,
                start.byte_pos,
                start.byte_pos + byte_len(field.bit)
            )
            .unwrap();
        }
        BuiltinTypes::U8 => {
            let end = start.next_pos(field.bit);
            assert!(start.byte_pos == end.byte_pos);

            if end.bit_pos < 7 {
                // add shift operation
                let mut bw = HeadTailWriter::new(&mut output, "(", ")");
                write!(
                    bw.get_writer(),
                    "{}[{}]>>{}",
                    target_slice,
                    start.byte_pos,
                    7 - end.bit_pos
                )
                .unwrap();
            } else {
                write!(output, "{}[{}]", target_slice, start.byte_pos).unwrap();
            }

            if field.bit < 8 {
                // add or
                write!(output, "&{}", ones_mask(0, field.bit - 1)).unwrap();
            }
        }
        BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
            if start.bit_pos == 0 && field.bit % 8 != 0 {
                let mut bw = HeadTailWriter::new(&mut output, "(", ")");
                {
                    let mut new = network_endian_read(bw.get_writer(), field.bit);
                    write!(
                        new.get_writer(),
                        "&{}[{}..{}]",
                        target_slice,
                        start.byte_pos,
                        start.byte_pos + byte_len(field.bit)
                    )
                    .unwrap();
                }
                write!(bw.get_writer(), ">>{}", 8 * byte_len(field.bit) - field.bit).unwrap();
            } else {
                let mut new = network_endian_read(&mut output, field.bit);
                write!(
                    new.get_writer(),
                    "&{}[{}..{}]",
                    target_slice,
                    start.byte_pos,
                    start.byte_pos + byte_len(field.bit)
                )
                .unwrap();
            }

            if field.bit % 8 != 0 {
                write!(output, "&{}", ones_mask(0, field.bit - 1)).unwrap();
            }
        }
        _ => panic!(),
    }
}

fn read_field_cross_byte(
    field: &Field,
    start: BitPos,
    target_slice: &str,
    mut output: &mut dyn Write,
) {
    let end = start.next_pos(field.bit);
    let do_write = |output: &mut dyn Write| {
        write!(
            output,
            "({}[{}]<<{})|({}[{}]>>{})",
            target_slice,
            start.byte_pos,
            field.bit - (8 - start.bit_pos),
            target_slice,
            end.byte_pos,
            7 - end.bit_pos
        )
        .unwrap();
    };

    if field.bit < 8 {
        // add shift operation
        let mut bw = HeadTailWriter::new(&mut output, "(", ")");
        do_write(bw.get_writer());
    } else {
        do_write(output);
    }

    if field.bit < 8 {
        // add or
        write!(output, "&{}", ones_mask(0, field.bit - 1)).unwrap();
    }
}

fn read_repr(field: &Field, start: BitPos, target_slice: &str, output: &mut dyn Write) {
    let end = start.next_pos(field.bit);
    if field.bit <= 8 && start.byte_pos != end.byte_pos {
        read_field_cross_byte(field, start, target_slice, output);
    } else {
        read_field(field, start, target_slice, output);
    }
}

fn read_as_arg(field: &Field, start: BitPos, target_slice: &str, mut output: &mut dyn Write) {
    match &field.arg {
        Arg::Code(_) => {
            // arg is code
            let mut into_writer = HeadTailWriter::new(&mut output, "(", ").into()");
            read_repr(field, start, target_slice, into_writer.get_writer());
        }
        Arg::BuiltinTypes(defined_arg) => {
            if *defined_arg == field.repr {
                // arg is the same as the repr
                read_repr(field, start, target_slice, output);
            } else {
                // arg is bool, field.bit == 1
                // A fast path method for accessing bool
                write!(
                    output,
                    "{}[{}]&{} != 0",
                    target_slice,
                    start.byte_pos,
                    ones_mask(7 - start.bit_pos, 7 - start.bit_pos)
                )
                .unwrap();
            }
        }
    }
}

fn field_get_method(
    name: &str,
    field: &Field,
    start: BitPos,
    target_slice: &str,
    mut output: &mut dyn Write,
) {
    writeln!(output, "#[inline]").unwrap();

    let mut func_def = if field.gen {
        "pub fn ".to_string()
    } else {
        "fn ".to_string()
    };
    func_def += &format!("{}(&self)->{}{{\n", name, field.arg.to_string());

    let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");
    read_as_arg(field, start, target_slice, func_def_writer.get_writer());
}

pub fn header_field_get_method(name: &str, field: &Field, start: BitPos, output: &mut dyn Write) {
    field_get_method(name, field, start, "self.buf.as_ref()", output)
}

pub fn packet_field_get_method(name: &str, field: &Field, start: BitPos, output: &mut dyn Write) {
    field_get_method(name, field, start, "self.buf.chunk()", output)
}

fn read_from_non_field(field: &Field, start: BitPos, target_slice: &str, mut output: &mut dyn Write) {
    assert!(field.bit % 8 != 0);

    match field.repr {
        BuiltinTypes::U8 => {
            
        },
        _ => panic!()
    }
}

// We can not handle cross byte here
fn write_target<'field, 'output>(
    field: &'field Field,
    start: BitPos,
    target_slice: &str,
    output: &'output mut dyn Write,
) -> HeadTailWriter<&'output mut dyn Write> {
    // we first check some assertions, they are only
    // added here to help me reason about the algorithm
    match field.repr {
        BuiltinTypes::ByteSlice => {
            assert!(field.bit % 8 == 0 && field.bit > 8);

            HeadTailWriter::new(
                output,
                &format!(
                    "(&mut {}[{}..{}]).copy_from_slice(",
                    target_slice,
                    start.byte_pos,
                    start.byte_pos + byte_len(field.bit)
                ),
                ");",
            )
        }
        // we do not handle the cross byte u8
        BuiltinTypes::U8 => {
            assert!(start.byte_pos == start.next_pos(field.bit).byte_pos);
            HeadTailWriter::new(
                output,
                &format!("{}[{}]=", target_slice, start.byte_pos),
                ";",
            )
        }
        BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
            assert!(start.bit_pos == 0 || start.next_pos(field.bit).bit_pos == 7);
            match byte_len(field.bit) {
                2 => HeadTailWriter::new(
                    output,
                    &format!(
                        "NetworkEndian::write_u16(&mut {}[{}..{}],",
                        target_slice,
                        start.byte_pos,
                        start.byte_pos + 2
                    ),
                    ");",
                ),
                3 => HeadTailWriter::new(
                    output,
                    &format!(
                        "NetworkEndian::write_u24(&mut {}[{}..{}],",
                        target_slice,
                        start.byte_pos,
                        start.byte_pos + 3
                    ),
                    ");",
                ),
                4 => HeadTailWriter::new(
                    output,
                    &format!(
                        "NetworkEndian::write_u32(&mut {}[{}..{}],",
                        target_slice,
                        start.byte_pos,
                        start.byte_pos + 4
                    ),
                    ");",
                ),
                5 | 6 | 7 => HeadTailWriter::new(
                    output,
                    &format!(
                        "NetworkEndian::write_uint(&mut {}[{}..{}],",
                        target_slice,
                        start.byte_pos,
                        start.byte_pos + byte_len(field.bit),
                    ),
                    &format!(",{});", byte_len(field.bit)),
                ),
                8 => HeadTailWriter::new(
                    output,
                    &format!(
                        "NetworkEndian::write_u64(&mut {}[{}..{}],",
                        target_slice,
                        start.byte_pos,
                        start.byte_pos + 8
                    ),
                    ");",
                ),
                _ => panic!(),
            }
        }
        _ => panic!(),
    }
}

#[cfg(test)]
mod codegen_tests {
    use crate::{
        parser,
        token::{self, Tokenizer},
    };

    use super::*;

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

    macro_rules! do_test_field_codegen {
        ($test_fn: ident, $target_slice: expr, $program: expr, $bit_pos: expr, $expected: expr) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_fn(&field, $bit_pos, $target_slice, &mut buf);
            assert_eq!($expected, std::str::from_utf8(&buf[..]).unwrap());
        };
    }

    macro_rules! print_field_codegen {
        ($test_fn: ident, $target_slice: expr, $program: expr, $bit_pos: expr, $expected: expr) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_fn(&field, $bit_pos, $target_slice, &mut buf);
            println!("{}", std::str::from_utf8(&buf[..]).unwrap())
        };
    }

    #[test]
    fn test_read_repr() {
        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 5}",
            BitPos {
                byte_pos: 0,
                bit_pos: 1,
            },
            "(self.buf.as_ref()[0]>>2)&0x1f"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 5}",
            BitPos {
                byte_pos: 0,
                bit_pos: 3,
            },
            "self.buf.as_ref()[0]&0x1f"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 8}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()[0]"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 3}",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
            "((self.buf.as_ref()[0]<<1)|(self.buf.as_ref()[1]>>7))&0x7"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 8}",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
            "(self.buf.as_ref()[0]<<6)|(self.buf.as_ref()[1]>>2)"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 9}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "(NetworkEndian::read_u16(&self.buf.as_ref()[0..2])>>7)&0x1ff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 14}",
            BitPos {
                byte_pos: 0,
                bit_pos: 2,
            },
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])&0x3fff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 16}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 16, repr = &[u8]}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "&self.buf.as_ref()[0..2]"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 22}",
            BitPos {
                byte_pos: 4,
                bit_pos: 2,
            },
            "NetworkEndian::read_u24(&self.buf.as_ref()[4..7])&0x3fffff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 29}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(NetworkEndian::read_u32(&self.buf.as_ref()[3..7])>>3)&0x1fffffff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 35}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(NetworkEndian::read_uint(&self.buf.as_ref()[3..8], 5)>>5)&0x7ffffffff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 46}",
            BitPos {
                byte_pos: 3,
                bit_pos: 2,
            },
            "NetworkEndian::read_uint(&self.buf.as_ref()[3..9], 6)&0x3fffffffffff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 55}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(NetworkEndian::read_uint(&self.buf.as_ref()[3..10], 7)>>1)&0x7fffffffffffff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 60}",
            BitPos {
                byte_pos: 3,
                bit_pos: 4,
            },
            "NetworkEndian::read_u64(&self.buf.as_ref()[3..11])&0xfffffffffffffff"
        );

        do_test_field_codegen!(
            read_repr,
            "self.buf.as_ref()",
            "Field {bit  = 128}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "&self.buf.as_ref()[3..19]"
        );
    }

    #[test]
    fn test_read_arg() {
        do_test_field_codegen!(
            read_as_arg,
            "self.buf.as_ref()",
            "Field {bit  = 32, repr = &[u8], arg = %%Ipv4Addr%%, default=%%Ipv4Addr::default()%%}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(&self.buf.as_ref()[3..7]).into()"
        );

        do_test_field_codegen!(
            read_as_arg,
            "self.buf.as_ref()",
            "Field {bit = 1, arg = bool, default = false}",
            BitPos {
                byte_pos: 13,
                bit_pos: 0,
            },
            "self.buf.as_ref()[13]&0x80 != 0"
        );
    }
}
