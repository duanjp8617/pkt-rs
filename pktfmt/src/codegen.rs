use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::ast::*;

struct BracketWriter<T: Write> {
    writer: T,
}

impl<T: Write> BracketWriter<T> {
    fn new(mut writer: T) -> Self {
        write!(writer, "(").unwrap();
        BracketWriter { writer }
    }

    fn get_writer(&mut self) -> &mut T {
        &mut self.writer
    }
}

impl<T: Write> Drop for BracketWriter<T> {
    fn drop(&mut self) {
        write!(&mut self.writer, ")").unwrap();
    }
}

fn bit_mask(mut low: u64, high: u64, t: BuiltinTypes) -> String {
    assert!(low <= high);
    match t {
        BuiltinTypes::U8 => assert!(high < 8),
        BuiltinTypes::U16 => assert!(high < 16),
        BuiltinTypes::U32 => assert!(high < 32),
        BuiltinTypes::U64 => assert!(high < 64),
        _ => panic!(),
    }

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

#[inline]
fn read_repr_8b(field: &Field, start: BitPos, target_slice: &str, mut output: &mut dyn Write) {
    assert!(field.bit <= 8 && field.repr == BuiltinTypes::U8);

    let end = start.next_pos(field.bit);
    if start.byte_pos == end.byte_pos {
        if end.bit_pos < 7 {
            // add shift operation
            let mut bw = BracketWriter::new(&mut output);
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
        let do_write = |start: BitPos, end: BitPos, target_slice: &str, output: &mut dyn Write| {
            write!(
                output,
                "({}[{}]<<{})|({}[{}]>>{})",
                target_slice,
                start.byte_pos,
                end.bit_pos + 1,
                target_slice,
                end.byte_pos,
                9 - start.bit_pos + end.bit_pos
            )
            .unwrap();
        };

        if field.bit < 8 {
            // add shift operation
            let mut bw = BracketWriter::new(&mut output);
            do_write(start, end, target_slice, bw.get_writer());
        } else {
            do_write(start, end, target_slice, output);
        }
    }

    if field.bit < 8 {
        // add or
        write!(output, "&{}", bit_mask(0, field.bit - 1, BuiltinTypes::U8)).unwrap();
    }
}

#[cfg(test)]
mod codegen_tests {
    use super::*;

    #[test]
    fn test_bracket_writer() {
        let mut s: Vec<u8> = ::std::vec::Vec::new();

        {
            let mut writer = BracketWriter::new(&mut s);
            write!(writer.get_writer(), "{}", 222).unwrap();
        }

        write!(&mut s, "{}", 555).unwrap();

        {
            let mut writer = BracketWriter::new(&mut s);
            write!(writer.get_writer(), "{}", 777).unwrap();
        }

        assert_eq!(std::str::from_utf8(&s[..]).unwrap(), "(222)555(777)");
    }

    #[test]
    fn test_bit_mask() {
        let s = bit_mask(14, 33, BuiltinTypes::U64);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(7, 22, BuiltinTypes::U64);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(55, 55, BuiltinTypes::U64);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(55, 63, BuiltinTypes::U64);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(44, 45, BuiltinTypes::U64);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );

        let s = bit_mask(0, 63, BuiltinTypes::U64);
        assert_eq!(
            s,
            format!("{:#x}", u64::from_str_radix(&s[2..], 16).unwrap())
        );
    }
}
