use std::fmt;

use byteorder::{ByteOrder, NetworkEndian};

use crate::codegen::byte_len;

mod number;
pub use number::*;

mod field;
pub use field::*;

mod header;
pub use header::*;

mod length;
pub use length::*;

/// The top level ast type for the `packet`` definition
#[derive(Debug)]
pub struct Packet {
    protocol_name: String,
    header: Header,
    length: Length,
    header_template: Vec<u8>,
}

impl Packet {
    pub fn new(protocol_name: &str, header: header::Header, length: length::Length) -> Self {
        let header_template = build_header_template(&header);
        Self {
            protocol_name: protocol_name.to_string(),
            header,
            length,
            header_template,
        }
    }

    pub fn protocol_name(&self) -> &str {
        &self.protocol_name
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn length(&self) -> &Length {
        &self.length
    }

    pub fn header_template(&self) -> &[u8] {
        &self.header_template
    }
}

// An enum type to describe the type of the error
#[derive(Debug, Clone, PartialEq, Eq)]
enum ErrorType {
    // 1 errors
    NumberError,
    // 6 errors
    FieldDef,
    // 4 errors
    HeaderDef,
    // 10 errors
    LengthDef,
}

// Record the error type and error index.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ErrorPos(ErrorType, usize);

impl fmt::Display for ErrorPos {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            ErrorType::NumberError => write!(fmt, "number error {}", self.1),
            ErrorType::FieldDef => write!(fmt, "field error {}", self.1),
            ErrorType::HeaderDef => write!(fmt, "header error {}", self.1),
            ErrorType::LengthDef => write!(fmt, "length error {}", self.1),
        }
    }
}

/// The ast-related error when parsing the pktfmt script.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Error {
    pos: ErrorPos,
    reason: String,
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:\n{}", self.pos, self.reason)
    }
}

impl Error {
    pub fn num_error(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::NumberError, index),
            reason,
        }
    }

    pub fn field(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::FieldDef, index),
            reason,
        }
    }

    pub fn header(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::HeaderDef, index),
            reason,
        }
    }

    pub fn length(index: usize, reason: String) -> Self {
        Self {
            pos: ErrorPos(ErrorType::LengthDef, index),
            reason,
        }
    }
}

// calculate the max value of `bit` bits for `u64`
pub(crate) fn max_value(bit: u64) -> Option<u64> {
    assert!(bit > 0);

    if bit > 64 {
        None
    } else if bit < 64 {
        Some((1 << bit) - 1)
    } else {
        Some(u64::MAX)
    }
}

macro_rules! predefined_header_u16_u32_u64_impl {
    ($write_func: ident, $repr: ident, $args:expr) => {{
        let (start, field, target_slice) = $args;
        if field.bit % 8 == 0 {
            let end = start.next_pos(field.bit);
            let default_val = match &field.default {
                DefaultVal::Num(b) => *b,
                _ => panic!(),
            };

            // The field has the form:
            // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
            // |   field                     |
            NetworkEndian::$write_func(
                &mut target_slice[start.byte_pos() as usize..end.byte_pos() as usize],
                default_val as $repr,
            );
        } else {
            let end = start.next_pos(field.bit);
            let default_val = match &field.default {
                DefaultVal::Num(b) => *b,
                _ => panic!(),
            };

            // The field has the form:
            // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
            // |   field       | | rest bits |

            if end.bit_pos() == 7 {
                // The field has the form:
                // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                // |rest bits| |   field         |
                // We do the following steps to read the
                // rest of the bits:
                // 1. Read the byte containing the rest of the bits ("{}[{}]").
                // 2. Remove the extra bits that belong to the field area ("{}[{}]&{}").
                // 3. Convert the value to `repr` type ("({}[{}]&{}) as {})")
                // 4. Left shift to make room for the field area ("(({}[{}]&{}) as {}) << {}")
                let mut bit_mask: u8 = 0x00;
                for i in (7 - start.bit_pos() + 1)..8 {
                    bit_mask = bit_mask & (1 << i);
                }
                let rest_of_field = ((target_slice[start.byte_pos() as usize] & bit_mask) as $repr)
                    << (8 * (byte_len(field.bit) - 1));

                NetworkEndian::$write_func(
                    &mut target_slice[start.byte_pos() as usize..end.byte_pos() as usize],
                    rest_of_field | (default_val as $repr),
                );
            } else {
                // The field has the form:
                // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                // |   field         | |rest bits|
                // We do similar steps except for the
                // final one (the left-shift one).
                let mut bit_mask: u8 = 0x00;
                for i in 0..(7 - end.bit_pos()) {
                    bit_mask = bit_mask & (1 << i);
                }
                let rest_of_field = (target_slice[end.byte_pos() as usize] & bit_mask) as $repr;
                NetworkEndian::$write_func(
                    &mut target_slice[start.byte_pos() as usize..end.byte_pos() as usize],
                    rest_of_field | ((default_val as $repr) << (7 - end.bit_pos())),
                );
            }
        }
    }};
}

fn build_header_template(header: &Header) -> Vec<u8> {
    let mut header_template = Vec::new();
    header_template.resize(header.header_len_in_bytes(), 0);

    for (_, field, start) in header.field_iter() {
        match &field.arg {
            Arg::BuiltinTypes(defined_arg) if *defined_arg != field.repr => {
                // Generate a fast path method in case that
                //`bit` is 1, `repr` is `U8` and `arg` is bool.
                // This will write 1 to the field bit if `write_value` is true,
                // and write 0 to the field bit if `write_value` is false.
                let target_slice = &mut header_template[..];
                let start_byte_pos = start.byte_pos() as usize;
                let default_val = match field.default {
                    DefaultVal::Bool(b) => b,
                    _ => panic!(),
                };

                if default_val {
                    target_slice[start_byte_pos] =
                        target_slice[start_byte_pos] | (1 << (7 - u64::from(start.bit_pos())))
                } else {
                    target_slice[start_byte_pos] =
                        target_slice[start_byte_pos] & (!(1 << (7 - u64::from(start.bit_pos()))))
                }
            }
            _ => {
                let end = start.next_pos(field.bit);
                if field.bit <= 8 && start.byte_pos() != end.byte_pos() {
                    let target_slice = &mut header_template[..];
                    let start_byte_pos = start.byte_pos() as usize;
                    let end_byte_pos = end.byte_pos() as usize;
                    let default_val = match field.default {
                        DefaultVal::Num(b) => b,
                        _ => panic!(),
                    };

                    // The field will have the following form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    //       |     fie-ld  |
                    // The field is splitted into two parts by the byte boundary:

                    // The 1st part is :
                    // 0 1 2 3 4 5 6 7
                    //       |  fie- |
                    // To write to the 1st part, we do the following steps:
                    // 1. Read the rest of the bits on the first part ("({}[{}]&{})")
                    // 2. Right shift the `write_value` ("({}>>{})")
                    // 3. Glue them together and write to the area covering the 1st part.
                    target_slice[start_byte_pos] = (target_slice[start_byte_pos]
                        & (!((1 << (7 - start.bit_pos() + 1)) - 1)))
                        | ((default_val as u8) >> (end.bit_pos() + 1));

                    // The 2nd part ("({}[{}]>>{})") is :
                    // 0 1 2 3 4 5 6 7
                    // |-ld|
                    // To write to the 2nd part, we do the following steps:
                    // 1. Read the rest of the bits on the 2nd part ("({}[{}]&{})")
                    // 2. Left shift the `write_value` ("({}<<{})")
                    // 3. Glue them together and write to the area covering the 2nd part.
                    target_slice[end_byte_pos] = (target_slice[end_byte_pos]
                        & ((1 << (7 - end.bit_pos())) - 1))
                        | ((default_val as u8) << (7 - end.bit_pos()));
                } else {
                    match &field.repr {
                        BuiltinTypes::ByteSlice => {
                            let target_slice = &mut header_template[..];
                            let start_byte_pos = start.byte_pos() as usize;
                            let end_byte_pos = end.byte_pos() as usize;
                            let default_val = match &field.default {
                                DefaultVal::Bytes(b) => b,
                                _ => panic!(),
                            };

                            // The `repr` is a `ByteSlice`.
                            // The field has the following form:
                            // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                            // |          field              |
                            // The field area contains no extra bits,
                            // we just write `write_value` to the field
                            // area.
                            let target_slice =
                                &mut target_slice[start_byte_pos as usize..end_byte_pos as usize];
                            target_slice.copy_from_slice(&default_val[..]);
                        }
                        BuiltinTypes::U8 => {
                            let target_slice = &mut header_template[..];
                            let start_byte_pos = start.byte_pos() as usize;
                            let default_val = match &field.default {
                                DefaultVal::Num(b) => *b,
                                _ => panic!(),
                            };

                            let write_target = &mut target_slice[start_byte_pos as usize];
                            if field.bit % 8 == 0 {
                                // The field has the following form:
                                // 0 1 2 3 4 5 6 7
                                // |     field   |
                                // We directly assign the `write_value` to the write target.
                                *write_target = default_val as u8;
                            } else {
                                // The field area contains extra bits and we
                                // extract the rest of the bits through a
                                // mask.
                                let mut bit_mask: u8 = 0xff;
                                for i in (7 - end.bit_pos())..(7 - start.bit_pos() + 1) {
                                    bit_mask = bit_mask & (!(1 << i));
                                }
                                let rest_of_bits = *write_target & bit_mask;

                                if end.bit_pos() == 7 {
                                    // The field has the following form:
                                    // 0 1 2 3 4 5 6 7
                                    //       | field |
                                    // `write_value` has the same form as
                                    // field.
                                    // We glue `rest_of_bits` with
                                    // `write_value` and write
                                    // to the `write_target`.
                                    *write_target = rest_of_bits | (default_val as u8);
                                } else {
                                    // The field has the following form:
                                    // 0 1 2 3 4 5 6 7
                                    // | field |
                                    // We left shift the `write_value` to
                                    // make room
                                    // for the rest of the bits.
                                    // Then we glue them together and write
                                    // to the
                                    // `write_target`.
                                    *write_target =
                                        rest_of_bits | ((default_val as u8) << (7 - end.bit_pos()));
                                }
                            }
                        }
                        BuiltinTypes::U16 => predefined_header_u16_u32_u64_impl!(
                            write_u16,
                            u16,
                            (start, field, &mut header_template[..])
                        ),
                        BuiltinTypes::U32 => predefined_header_u16_u32_u64_impl!(
                            write_u32,
                            u32,
                            (start, field, &mut header_template[..])
                        ),
                        BuiltinTypes::U64 => predefined_header_u16_u32_u64_impl!(
                            write_u64,
                            u64,
                            (start, field, &mut header_template[..])
                        ),
                        _ => panic!(),
                    }
                }
            }
        }
    }

    header_template
}
