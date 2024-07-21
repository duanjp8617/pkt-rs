use std::collections::HashMap;

use crate::utils::Spanned;

use super::field::Field;
use super::Error;

const RESERVED_FIELD_NAMES: &[&str] = &["header_len", "payload_len", "packet_len"];

/// Given a parsed header with a list of fields, check its correctness and
/// construct data structures for efficient header field queries.
///
/// Input arguments:
/// 
/// `field_list`: a list consists of user-defined header fields.
/// 
/// `header_pos`: the byte indexes of the header list in the original file
/// 
/// Return value:
/// 
/// Either `Ok((field_list, field_position))` or `Err((error, (left, right)))`.
/// 
/// `field_list`: an list that preserves the order of the header fields, with each element being the field name and the field object, used for field object indexing
/// 
/// `field_position`: a hashmap that maps the field name to the bit position and field list index
/// 
/// `error`: the error generated if correctness check fails
/// 
/// `left` and `right`: the byte indexes of the header list
pub fn check_header_list(
    field_list: Vec<(Spanned<String>, Field)>,
    header_pos: (usize, usize),
) -> Result<(Vec<(String, Field)>, HashMap<String, (BitPos, usize)>), (Error, (usize, usize))> {
    // field_name -> (bit position within the header, index of the field list)
    let mut field_position = HashMap::new();

    // temporary variable for recording the field bit position
    let mut global_bit_pos = 0;

    let field_list = field_list
        .into_iter()
        .enumerate()
        .map(|(field_idx, (sp_str, field))| {
            if field_position.get(&sp_str.item).is_some() {
                // header list correctness check 1:
                // header field name can't be duplicated
                let reason = format!("duplicated header field name {}", &sp_str.item);
                return_err_1!((Error::header(reason), sp_str.span))
            } else if RESERVED_FIELD_NAMES
                .iter()
                .find(|reserved| **reserved == &sp_str.item)
                .is_some()
            {
                // header list correctness check 1:
                // header field name can't be any of the reserved names
                let reason = format!(
                    "header field name {} is reserved and can't be used",
                    &sp_str.item
                );
                return_err_1!((Error::header(reason), sp_str.span))
            } else {
                // calculate the start and end bit position of the header
                let start = BitPos::new(global_bit_pos);
                let end = start.next_pos(field.bit);

                if field.bit > 8 && start.bit_pos != 0 && end.bit_pos != 7 {
                    // If the header field contains multiple bytes, then one of two ends must be
                    // aligned to the byte boudary. In this branch, neither of
                    // the two ends are aligned to the byte boundary, we report an error.
                    let reason = format!(
                        "header field {} is not correctly aligned to the byte boundaries",
                        &sp_str.item
                    );
                    return_err_1!((Error::header(reason), sp_str.span))
                } else {
                    // update global bit pos
                    global_bit_pos += field.bit;
                    // update field position
                    field_position.insert(sp_str.item.clone(), (start, field_idx));
                    Ok((sp_str.item, field))
                }
            }
        })
        .collect::<Result<Vec<_>, (Error, (usize, usize))>>()?;

    // Next, we check whether the total bit size of the header aligns to the byte
    // boundary
    if global_bit_pos % 8 != 0 {
        let reason = format!("invalid header bit length {}", global_bit_pos);
        return_err_1!((Error::header(reason), header_pos))
    } else {
        Ok((field_list, field_position))
    }
}

/// BitPos records the starting and ending position of a header field.
///
/// An example of the header field position:
/// ```
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
    pub byte_pos: u64,
    pub bit_pos: u64,
}

impl BitPos {
    // Calculate the BitPos from the global bit pos
    pub(crate) fn new(global_bit_pos: u64) -> Self {
        Self {
            byte_pos: global_bit_pos / 8,
            bit_pos: global_bit_pos % 8,
        }
    }

    // Calculate the corresponding global bit pos
    pub(crate) fn to_global_pos(&self) -> u64 {
        self.byte_pos * 8 + self.bit_pos
    }

    // Calculate the next BitPos that is `len` bits away from the current BitPos
    pub(crate) fn next_pos(&self, len: u64) -> Self {
        Self::new(self.to_global_pos() + len - 1)
    }
}


