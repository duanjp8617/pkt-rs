use std::io::Write;

use byteorder::{ByteOrder, NetworkEndian};

use crate::ast::{Arg, BuiltinTypes, DefaultVal, Packet};

use super::{byte_len, GenerateFieldAccessMethod, HeadTailWriter};

// macro_rules! predefined_header_u16_u32_u64_impl {
//     ($repr: ident, $start: expr, $field: expr) => {
//         let end = $start.next_pos($field.bit);
//         let default_val = match &$field.default {
//             DefaultVal::Num(b) => *b,
//             _ => panic!(),
//         };

//         if $field.bit % 8 == 0 {
//             // The field has the form:
//             // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
//             // |   field                     |
//             NetworkEndian::write_$repr(
//                 &mut target_slice
//                     [start.byte_pos() as usize..(start.byte_pos() +
// byte_len(field.bit)) as usize],                 default_val as $repr,
//             );
//         } else {
//             // The field has the form:
//             // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
//             // |   field       | | rest bits |

//             if end.bit_pos() == 7 {
//                 // The field has the form:
//                 // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
//                 // |rest bits| |   field         |
//                 // We do the following steps to read the
//                 // rest of the bits:
//                 // 1. Read the byte containing the rest of the bits
// ("{}[{}]").                 // 2. Remove the extra bits that belong to the
// field area ("{}[{}]&{}").                 // 3. Convert the value to `repr`
// type ("({}[{}]&{}) as {})")                 // 4. Left shift to make room for
// the field area ("(({}[{}]&{}) as {}) << {}")                 let mut
// bit_mask: u8 = 0x00;                 for i in (7 - start.bit_pos() + 1)..8 {
//                     bit_mask = bit_mask & (1 << i);
//                 }
//                 let rest_of_field = ((target_slice[start.byte_pos() as usize]
// & bit_mask) as $repr)                     << (8 * (byte_len(field.bit) - 1));

//                 NetworkEndian::write_$repr(
//                     &mut target_slice[start.byte_pos() as usize
//                         ..(start.byte_pos() + byte_len(field.bit)) as usize],
//                     rest_of_field | (default_val as $repr),
//                 );
//             } else {
//                 // The field has the form:
//                 // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
//                 // |   field         | |rest bits|
//                 // We do similar steps except for the
//                 // final one (the left-shift one).
//                 let mut bit_mask: u8 = 0x00;
//                 for i in 0..(7 - end.bit_pos()) {
//                     bit_mask = bit_mask & (1 << i);
//                 }
//                 let rest_of_field = (target_slice[end.byte_pos() as usize] &
// bit_mask) as $repr;                 NetworkEndian::write_$repr(
//                     &mut target_slice[start.byte_pos() as usize
//                         ..(start.byte_pos() + byte_len(field.bit)) as usize],
//                     rest_of_field | ((default_val as $repr) << (7 -
// end.bit_pos())),                 );
//             }
//         }
//     };
// }

pub struct HeaderImpl<'a> {
    packet: &'a Packet,
}

impl<'a> HeaderImpl<'a> {
    pub fn new(packet: &'a Packet) -> Self {
        Self { packet }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        // Generate a constant for the fixed header length.
        self.header_len_gen(output);

        // Generate a byte array containing a pre-defined header
        // whose field values are set to default.
        // TODO:
        // self.predefined_header(output);

        // Defines the header struct.
        let header_struct_gen = StructDefinition {
            struct_name: &self.header_struct_name(),
            derives: &["Debug", "Clone", "Copy"],
        };
        header_struct_gen.code_gen(output);

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]>",
                &self.header_struct_name(),
                "T",
                &mut output,
            );

            // Generate the `parse` and `parse_unchecked` methods.
            self.parse(impl_block.get_writer());
            StructDefinition::parse_unchecked(impl_block.get_writer());

            // Generate `as_bytes` and `to_owned` methods.
            self.as_bytes(impl_block.get_writer());
            self.to_owned(impl_block.get_writer());

            // Generate get methods for various header fields.
            self.packet
                .header_field_method_gen("self.buf.as_ref()", None, impl_block.get_writer());

            // Generate get methods for length fields if there are any.
            self.packet
                .length_field_method_gen("self.buf.as_ref()", None, impl_block.get_writer());
        }

        {
            let mut impl_block = impl_block(
                "T:AsMut<[u8]>",
                &self.header_struct_name(),
                "T",
                &mut output,
            );

            // Generate set methods for various header fields.
            self.packet.header_field_method_gen(
                "self.buf.as_mut()",
                Some("value"),
                impl_block.get_writer(),
            );

            // Generate set methods for length fields if there are any.
            self.packet.length_field_method_gen(
                "self.buf.as_mut()",
                Some("value"),
                impl_block.get_writer(),
            );
        }
    }

    // Return the name of the header length const.
    fn header_len_name(&self) -> String {
        self.packet.protocol_name().to_uppercase() + "_HEADER_LEN"
    }

    fn predefined_header(&self) -> Vec<u8> {
        let packet = self.packet;

        let mut header_template = Vec::new();
        header_template.resize(packet.header().header_len_in_bytes(), 0);
        let target_slice = &mut header_template[..];

        for (_, field, start) in packet.header().field_iter() {
            match &field.arg {
                Arg::BuiltinTypes(defined_arg) if *defined_arg != field.repr => {
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
                            & (!((1 << (8 - start.bit_pos())) - 1)))
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
                        // self.write_field(target_slice, write_value, output);
                        match &field.repr {
                            BuiltinTypes::ByteSlice => {
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
                                    ..(start.byte_pos() + field.bit) as usize];
                                target_slice.copy_from_slice(&default_val[..]);
                            }
                            BuiltinTypes::U8 => {
                                let end = start.next_pos(field.bit);
                                let default_val = match &field.default {
                                    DefaultVal::Num(b) => *b,
                                    _ => panic!(),
                                };

                                let write_target = &mut target_slice[start.byte_pos() as usize];
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
                            BuiltinTypes::U16 => {
                                let end = start.next_pos(field.bit);
                                let default_val = match &field.default {
                                    DefaultVal::Num(b) => *b,
                                    _ => panic!(),
                                };

                                if field.bit % 8 == 0 {
                                    // The field has the form:
                                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                                    // |   field                     |
                                    NetworkEndian::write_u16(
                                        &mut target_slice[start.byte_pos() as usize
                                            ..(start.byte_pos() + byte_len(field.bit)) as usize],
                                        default_val as u16,
                                    );
                                } else {
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
                                                as u16)
                                                << (8 * (byte_len(field.bit) - 1));

                                        NetworkEndian::write_u16(
                                            &mut target_slice[start.byte_pos() as usize
                                                ..(start.byte_pos() + byte_len(field.bit))
                                                    as usize],
                                            rest_of_field | (default_val as u16),
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
                                            as u16;
                                        NetworkEndian::write_u16(
                                            &mut target_slice[start.byte_pos() as usize
                                                ..(start.byte_pos() + byte_len(field.bit))
                                                    as usize],
                                            rest_of_field
                                                | ((default_val as u16) << (7 - end.bit_pos())),
                                        );
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        header_template
    }

    // Return the name of the header struct.
    fn header_struct_name(&self) -> String {
        self.packet.protocol_name().to_string() + "Header"
    }

    fn header_len_gen(&self, output: &mut dyn Write) {
        let header_len = self.packet.header().header_len_in_bytes();

        write!(
            output,
            "/// A constant that defines the fixed byte length of the {} protocol header.
pub const {}: usize = {header_len};
",
            &self.packet.protocol_name(),
            self.header_len_name(),
        )
        .unwrap();
    }

    fn parse(&self, output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn parse(buf: T) -> Result<Self, T>{{
if buf.as_ref().len()>={}{{
Ok(Self{{buf}})
}} else {{
Err(buf)
}}
}}
",
            self.header_len_name()
        )
        .unwrap();
    }

    fn as_bytes(&self, output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn as_bytes(&self) -> &[u8]{{
&self.buf.as_ref()[0..{}]
}}
",
            self.header_len_name()
        )
        .unwrap();
    }

    fn to_owned(&self, output: &mut dyn Write) {
        let header_struct_name = self.header_struct_name();
        let header_len_name = self.header_len_name();

        write!(
            output,
            "#[inline]
pub fn to_owned(&self) -> {header_struct_name}<[u8; {header_len_name}]>{{
let mut buf = [0; {header_len_name}];
buf.copy_from_slice(self.as_bytes());
{header_struct_name} {{buf}}
}}
"
        )
        .unwrap();
    }
}

// Generate an implementation block for header/packet/message struct type.
fn impl_block<'out>(
    trait_name: &str,
    type_name: &str,
    type_param: &str,
    output: &'out mut dyn Write,
) -> HeadTailWriter<&'out mut dyn Write> {
    HeadTailWriter::new(
        output,
        &format!("impl<{trait_name}> {type_name}<{type_param}>{{\n"),
        "}\n",
    )
}

struct StructDefinition<'a> {
    struct_name: &'a str,
    derives: &'a [&'static str],
}

impl<'a> StructDefinition<'a> {
    fn code_gen(&self, mut output: &mut dyn Write) {
        assert!(self.derives.len() > 0);
        {
            let mut derive_writer = HeadTailWriter::new(&mut output, "#[derive(", ")]\n");
            self.derives
                .iter()
                .enumerate()
                .for_each(|(idx, derive_name)| {
                    write!(derive_writer.get_writer(), "{derive_name}").unwrap();
                    if idx < self.derives.len() - 1 {
                        write!(derive_writer.get_writer(), ",").unwrap();
                    }
                });
        }
        write!(
            output,
            "pub struct {}<T> {{
    buf: T
}}
",
            self.struct_name
        )
        .unwrap();
    }

    // Wrap a `buf` inside a packet struct.
    fn parse_unchecked(output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn parse_unchecked(buf: T) -> Self{{
Self{{buf}}
}}
"
        )
        .unwrap();
    }

    // Return an imutable reference to the contained `buf`.
    fn buf(output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn buf(&self) -> &T{{
&self.buf
}}
"
        )
        .unwrap();
    }

    // Release the `buf` from the containing packet struct.
    fn release(output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn release(self) -> T{{
self.buf
}}
"
        )
        .unwrap();
    }
}
