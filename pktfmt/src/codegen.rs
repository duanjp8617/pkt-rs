use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::ast::*;

// A helper that converts length in bit to length in bytes
fn byte_len(bit_len: u64) -> u64 {
    if bit_len % 8 == 0 {
        bit_len / 8
    } else {
        bit_len / 8 + 1
    }
}

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

fn network_endian_writer<T: Write>(writer: T, bit_len: u64) -> HeadTailWriter<T> {
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

fn bit_mask(mut low: u64, high: u64) -> String {
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

fn read_repr_8b(field: &Field, start: BitPos, target_slice: &str, mut output: &mut dyn Write) {
    assert!(field.bit <= 8 && field.repr == BuiltinTypes::U8);

    let end = start.next_pos(field.bit);
    if start.byte_pos == end.byte_pos {
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
    } else {
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
    }

    if field.bit < 8 {
        // add or
        write!(output, "&{}", bit_mask(0, field.bit - 1)).unwrap();
    }
}

fn read_repr_other(field: &Field, start: BitPos, target_slice: &str, mut output: &mut dyn Write) {
    assert!(field.bit > 8 && (start.bit_pos == 0 || start.next_pos(field.bit).bit_pos == 7));

    if field.repr == BuiltinTypes::ByteSlice {
        write!(
            output,
            "&{}[{}..{}]",
            target_slice,
            start.byte_pos,
            start.byte_pos + byte_len(field.bit)
        )
        .unwrap();
    } else {
        if start.bit_pos == 0 && field.bit % 8 != 0 {
            let mut bw = HeadTailWriter::new(&mut output, "(", ")");
            {
                let mut new = network_endian_writer(bw.get_writer(), field.bit);
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
            let mut new = network_endian_writer(&mut output, field.bit);
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
            write!(output, "&{}", bit_mask(0, field.bit - 1)).unwrap();
        }
    }
}

fn read_repr(field: &Field, start: BitPos, target_slice: &str, output: &mut dyn Write) {
    if field.bit <= 8 {
        read_repr_8b(field, start, target_slice, output);
    } else {
        read_repr_other(field, start, target_slice, output);
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
                    bit_mask(7 - start.bit_pos, 7 - start.bit_pos)
                )
                .unwrap();
            }
        }
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
        let s = bit_mask(14, 33);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(7, 22);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(55, 55);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(55, 63);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(44, 45);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(0, 63);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );
    }

    macro_rules! do_test_read {
        ($test_fn: ident, $target_slice: expr, $program: expr, $bit_pos: expr, $expected: expr) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_fn(&field, $bit_pos, $target_slice, &mut buf);
            assert_eq!($expected, std::str::from_utf8(&buf[..]).unwrap());
        };
    }

    macro_rules! print_read {
        ($test_fn: ident, $target_slice: expr, $program: expr, $bit_pos: expr, $expected: expr) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_fn(&field, $bit_pos, $target_slice, &mut buf);
            println!("{}", std::str::from_utf8(&buf[..]).unwrap())
        };
    }

    #[test]
    fn test_read_repr_8b() {
        do_test_read!(
            read_repr_8b,
            "self.buf.as_ref()",
            "Field {bit  = 5}",
            BitPos {
                byte_pos: 0,
                bit_pos: 1,
            },
            "(self.buf.as_ref()[0]>>2)&0x1f"
        );

        do_test_read!(
            read_repr_8b,
            "self.buf.as_ref()",
            "Field {bit  = 5}",
            BitPos {
                byte_pos: 0,
                bit_pos: 3,
            },
            "self.buf.as_ref()[0]&0x1f"
        );

        do_test_read!(
            read_repr_8b,
            "self.buf.as_ref()",
            "Field {bit  = 8}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "self.buf.as_ref()[0]"
        );

        do_test_read!(
            read_repr_8b,
            "self.buf.as_ref()",
            "Field {bit  = 3}",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
            "((self.buf.as_ref()[0]<<1)|(self.buf.as_ref()[1]>>7))&0x7"
        );

        do_test_read!(
            read_repr_8b,
            "self.buf.as_ref()",
            "Field {bit  = 8}",
            BitPos {
                byte_pos: 0,
                bit_pos: 6,
            },
            "(self.buf.as_ref()[0]<<6)|(self.buf.as_ref()[1]>>2)"
        );
    }

    #[test]
    fn test_read_repr_other() {
        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 9}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "(NetworkEndian::read_u16(&self.buf.as_ref()[0..2])>>7)&0x1ff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 14}",
            BitPos {
                byte_pos: 0,
                bit_pos: 2,
            },
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])&0x3fff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 16}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "NetworkEndian::read_u16(&self.buf.as_ref()[0..2])"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 16, repr = &[u8]}",
            BitPos {
                byte_pos: 0,
                bit_pos: 0,
            },
            "&self.buf.as_ref()[0..2]"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 22}",
            BitPos {
                byte_pos: 4,
                bit_pos: 2,
            },
            "NetworkEndian::read_u24(&self.buf.as_ref()[4..7])&0x3fffff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 29}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(NetworkEndian::read_u32(&self.buf.as_ref()[3..7])>>3)&0x1fffffff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 35}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(NetworkEndian::read_uint(&self.buf.as_ref()[3..8], 5)>>5)&0x7ffffffff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 46}",
            BitPos {
                byte_pos: 3,
                bit_pos: 2,
            },
            "NetworkEndian::read_uint(&self.buf.as_ref()[3..9], 6)&0x3fffffffffff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 55}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(NetworkEndian::read_uint(&self.buf.as_ref()[3..10], 7)>>1)&0x7fffffffffffff"
        );

        do_test_read!(
            read_repr_other,
            "self.buf.as_ref()",
            "Field {bit  = 60}",
            BitPos {
                byte_pos: 3,
                bit_pos: 4,
            },
            "NetworkEndian::read_u64(&self.buf.as_ref()[3..11])&0xfffffffffffffff"
        );

        do_test_read!(
            read_repr_other,
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
        do_test_read!(
            read_as_arg,
            "self.buf.as_ref()",
            "Field {bit  = 32, repr = &[u8], arg = %%Ipv4Addr%%, default=%%Ipv4Addr::default()%%}",
            BitPos {
                byte_pos: 3,
                bit_pos: 0,
            },
            "(&self.buf.as_ref()[3..7]).into()"
        );

        do_test_read!(
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
