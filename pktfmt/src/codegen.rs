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

    fn write<S: AsRef<str>>(&mut self, s: S) {
        write!(&mut self.writer, "{}", s.as_ref()).unwrap();
    }
}

impl<T: Write> Drop for BracketWriter<T> {
    fn drop(&mut self) {
        write!(&mut self.writer, ")").unwrap();
    }
}

#[inline]
fn read_field_8b(
    target_slice: &str,
    field: &Field,
    start: BitPos,
    end: BitPos,
    output: &mut dyn Write,
) {
    assert!(end.to_global_pos() - start.to_global_pos() <= 8 && field.repr == BuiltinTypes::U8);

    if start.byte_pos == end.byte_pos {
        write!(output, "{}[{}]", target_slice, start.byte_pos).unwrap();
        if end.bit_pos < 7 {
            // add shift operation
        }
        if field.bit < 8 {
            // add or
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bracket_writer() {
        let mut s: Vec<u8> = ::std::vec::Vec::new();

        {
            let mut writer = BracketWriter::new(&mut s);

            writer.write(format!("{}", 222));
        }

        write!(&mut s, "{}", 555).unwrap();

        {
            let mut writer = BracketWriter::new(&mut s);

            writer.write(format!("{}", 777));
        }

        println!("{}", std::str::from_utf8(&s[..]).unwrap());
    }
}
