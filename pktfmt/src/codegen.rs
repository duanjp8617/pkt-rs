use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::ast::*;

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
