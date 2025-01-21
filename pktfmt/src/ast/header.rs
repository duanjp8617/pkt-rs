use std::collections::HashMap;

use byteorder::{ByteOrder, NetworkEndian};

use crate::utils::{byte_len, Spanned};

use super::field::{Arg, BuiltinTypes, DefaultVal, Field};
use super::number::MAX_MTU_IN_BYTES;
use super::Error;

/// The ast type constructed when parsing `header` list from the pktfmt script.
///
/// **Member fields:**  
/// `header_len_in_byets`: the length of the fixe header in bytes  
/// `field_list`: an list that preserves the order of the header fields, with
/// each element being the field name and the field object, used for field
/// object indexing  
/// `field_position`: a hashmap that maps the field name to the bit position and
/// field list index
#[derive(Debug)]
pub struct Header {
    header_len_in_bytes: usize,
    field_list: Vec<(String, Field)>,
    field_position: HashMap<String, (BitPos, usize)>,
    header_template: Vec<u8>,
}

impl Header {
    /// Create a new `Header` object from the parsed input.
    ///
    /// **Input args:**  
    /// `field_list`: the parsed `header` list  
    /// `header_pos`: the byte indexes of the `header`` list in the original
    /// file
    ///
    /// **Return value:**  
    /// if succeed: a new `Header` object,  
    /// if fail: an error and the file indexes that triggers the error.
    pub fn new(
        field_list: Vec<(Spanned<String>, Field)>,
        header_pos: (usize, usize),
    ) -> Result<Self, (Error, (usize, usize))> {
        // field_name -> (bit position within the header, index of the field list)
        let mut field_position = HashMap::new();

        // temporary variable for recording the field bit position
        let mut global_bit_pos = 0;

        let field_list = field_list
            .into_iter()
            .enumerate()
            .map(|(field_idx, (sp_str, field))| {
                if field_position.get(&sp_str.item).is_some() {
                    // header error 1
                    return_err!((
                        Error::header(1, format!("duplicated header field name {}", &sp_str.item)),
                        sp_str.span
                    ))
                } else {
                    // calculate the start and end bit position of the header
                    let start = BitPos::new(global_bit_pos);
                    let end = start.next_pos(field.bit);

                    if field.bit > 8 && start.bit_pos != 0 && end.bit_pos != 7 {
                        // header error 2
                        // If the header field contains multiple bytes, then one of two ends must 
                        // be aligned to the byte boudary. In this branch, neither of the two ends
                        // are aligned to the byte boundary, we report an error.
                        return_err!((
                            Error::header(
                                2,
                                format!(
                                    "header field {} is not correctly aligned to the byte boundaries",
                                    &sp_str.item
                                )
                            ),
                            sp_str.span
                        ))
                    } else {
                        global_bit_pos += field.bit;
                        if global_bit_pos / 8 > MAX_MTU_IN_BYTES {
                            // header error 4
                            return_err!((
                                Error::header(
                                    4,
                                    format!(
                                        "header byte length is at least {}, exceeding the maximum MTU size {}",
                                        global_bit_pos / 8,
                                        MAX_MTU_IN_BYTES
                                    )
                                ),
                                header_pos
                            ))
                        }
                        else {
                            field_position.insert(sp_str.item.clone(), (start, field_idx));
                            Ok((sp_str.item, field))
                        }
                    }
                }
            })
            .collect::<Result<Vec<_>, (Error, (usize, usize))>>()?;

        if global_bit_pos % 8 != 0 {
            // header error 3
            return_err!((
                Error::header(
                    3,
                    format!(
                        "invalid header bit length {}, not dividable by 8",
                        global_bit_pos
                    )
                ),
                header_pos
            ))
        } else {
            let mut header = Self {
                header_len_in_bytes: (global_bit_pos / 8) as usize,
                field_list,
                field_position,
                header_template: Vec::new(),
            };
            header.build_header_template();

            Ok(header)
        }
    }

    /// Return an iterator that generates each `Field` and start `BitPos` of
    /// each `Field`.
    pub fn field_iter(&self) -> impl Iterator<Item = (&str, &Field, BitPos)> {
        self.field_list
            .iter()
            .map(|(name, field)| (&name[..], field, self.field_position.get(name).unwrap().0))
    }

    /// Given a field name `s`, return the corresponding `Field`.
    pub fn field(&self, s: &'_ str) -> Option<(&Field, BitPos)> {
        let (bit_pos, index) = self.field_position.get(s)?;

        Some((&self.field_list[*index].1, *bit_pos))
    }

    /// Get the length of the fixed header in bytes.
    pub fn header_len_in_bytes(&self) -> usize {
        self.header_len_in_bytes
    }

    /// Get the header template
    pub fn header_template(&self) -> &[u8] {
        &self.header_template[..]
    }

    fn build_header_template(&mut self) {
        let mut header_template = Vec::new();
        header_template.resize(self.header_len_in_bytes(), 0);

        for (_, field, start) in self.field_iter() {
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
                        target_slice[start_byte_pos] = target_slice[start_byte_pos]
                            & (!(1 << (7 - u64::from(start.bit_pos()))))
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
                                let target_slice = &mut target_slice[start.byte_pos() as usize
                                    ..(start.byte_pos() + byte_len(field.bit)) as usize];
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
                                        *write_target = rest_of_bits
                                            | ((default_val as u8) << (7 - end.bit_pos()));
                                    }
                                }
                            }
                            BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
                                let target_slice = &mut header_template[..];
                                if field.bit % 8 == 0 {
                                    let default_val = match &field.default {
                                        DefaultVal::Num(b) => *b,
                                        _ => panic!(),
                                    };

                                    // The field has the form:
                                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                                    // |   field                     |
                                    NetworkEndian::write_uint(
                                        &mut target_slice[start.byte_pos() as usize
                                            ..(start.byte_pos() + byte_len(field.bit)) as usize],
                                        default_val as u64,
                                        byte_len(field.bit) as usize,
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
                                        // 1. Read the byte containing the rest of the bits
                                        //    ("{}[{}]").
                                        // 2. Remove the extra bits that belong to the field area
                                        //    ("{}[{}]&{}").
                                        // 3. Convert the value to `repr` type ("({}[{}]&{}) as
                                        //    {})")
                                        // 4. Left shift to make room for the field area
                                        //    ("(({}[{}]&{}) as {}) << {}")
                                        let mut bit_mask: u8 = 0x00;
                                        for i in (7 - start.bit_pos() + 1)..8 {
                                            bit_mask = bit_mask & (1 << i);
                                        }
                                        let rest_of_field =
                                            ((target_slice[start.byte_pos() as usize] & bit_mask)
                                                as u64)
                                                << (8 * (byte_len(field.bit) - 1));

                                        NetworkEndian::write_uint(
                                            &mut target_slice[start.byte_pos() as usize
                                                ..(start.byte_pos() + byte_len(field.bit))
                                                    as usize],
                                            rest_of_field | (default_val as u64),
                                            byte_len(field.bit) as usize,
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
                                        let rest_of_field = (target_slice[end.byte_pos() as usize]
                                            & bit_mask)
                                            as u64;
                                        NetworkEndian::write_uint(
                                            &mut target_slice[start.byte_pos() as usize
                                                ..(start.byte_pos() + byte_len(field.bit))
                                                    as usize],
                                            rest_of_field
                                                | ((default_val as u64) << (7 - end.bit_pos())),
                                            byte_len(field.bit) as usize,
                                        );
                                    }
                                }
                            }
                            _ => panic!(),
                        }
                    }
                }
            }
        }
        self.header_template = header_template;
    }
}

/// BitPos records the starting and ending position of a header field.
///
/// An example of the header field position:
/// ```text
/// byte position:  0               1
/// bit position:   0 1 2 3 4 5 6 7 0 1 2  3  4  5  6  7
/// global bit pos: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
///                 ^                 ^
///           start BitPos       end BitPos
/// ```
/// Note: two BitPos can form a range, indicating the starting position and
/// ending position of the header field. This range is includsive by default.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct BitPos {
    byte_pos: u64,
    bit_pos: u8,
}

impl BitPos {
    pub(crate) fn new(global_bit_pos: u64) -> Self {
        Self {
            byte_pos: global_bit_pos / 8,
            bit_pos: (global_bit_pos % 8) as u8,
        }
    }

    pub(crate) fn to_global_pos(&self) -> u64 {
        self.byte_pos * 8 + (self.bit_pos as u64)
    }

    pub(crate) fn next_pos(&self, len: u64) -> Self {
        Self::new(self.to_global_pos() + len - 1)
    }

    pub(crate) fn byte_pos(&self) -> u64 {
        self.byte_pos
    }

    pub(crate) fn bit_pos(&self) -> u8 {
        self.bit_pos
    }
}
