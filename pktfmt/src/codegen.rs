use std::collections::HashMap;
use std::fmt::write;
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

use crate::ast::*;

const REST_OF_FIELD: &str = "rest_of_field";

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
// input `writer`.
// We use the `byteorder` crate just like `smoltcp` here.
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

// Generate bit mask with all ones from `low`-th bit
// to the `high`-th bit.
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

// Generate bit mask with all zeros from `low`-th bit
// to the `high`-th bit.
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

struct FieldGetMethod<'a> {
    field: &'a Field,
    start: BitPos,
}

impl<'a> FieldGetMethod<'a> {
    // Generate a get method to access the field with name `field_name` from the buffer slice
    // `target_slice`.
    // The generated method is written to `output`.
    fn code_gen(&self, field_name: &str, target_slice: &str, mut output: &mut dyn Write) {
        if self.field.gen {
            // We only generate the get method if `gen` is true

            // Generate function definition for a field get method.
            // It will generate:
            // pub fn field_name(&self) -> FieldArgType {
            // ...
            // }
            // writeln!(output, "#[inline]").unwrap();
            let func_def = format!(
                "#[inline]\npub fn {field_name}(&self)->{}{{\n",
                self.field.arg.to_string()
            );
            let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

            // Fill in the function body for a field get method.
            self.read_as_arg(target_slice, func_def_writer.get_writer());
        }
    }

    // Generae a code piece that read the field from the `target_slice` into a
    // `repr`-typed value.
    //
    // Note: this method does not handle the condition that the `repr` is `U8` and the
    // field crosses the byte boundary.
    fn read_field(&self, target_slice: &str, mut output: &mut dyn Write) {
        // The ending `BitPos` of the current header field.
        let end = self.start.next_pos(self.field.bit);

        match self.field.repr {
            BuiltinTypes::ByteSlice => {
                // The `repr` is a `ByteSlice`.
                // The field has the following form:
                // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                // |          field              |
                // The field covers the entire byte slice and can be directly read out.
                write!(
                    output,
                    "&{target_slice}[{}..{}]",
                    self.start.byte_pos,
                    self.start.byte_pos + byte_len(self.field.bit)
                )
                .unwrap();
            }
            BuiltinTypes::U8 => {
                // The `repr` is a `U8`.
                // The following assertion shows that the field locates within
                // a single byte and does not cross the byte boundaries.
                assert!(self.start.byte_pos == end.byte_pos);

                // Index the actual byte containing the field.
                let read_byte = format!("{target_slice}[{}]", self.start.byte_pos);

                if end.bit_pos < 7 && self.start.bit_pos > 0 {
                    // The field has the following form:
                    // 0 1 2 3 4 5 6 7
                    //   | field |
                    // We perform a right shift followed by bitwise and.
                    // This will clear the extra bits on the target byte
                    // and align the field to the 7th bit position.
                    write!(
                        output,
                        "({read_byte}>>{})&{}",
                        7 - end.bit_pos,
                        ones_mask(0, self.field.bit - 1)
                    )
                    .unwrap();
                } else if end.bit_pos < 7 {
                    // The field has the following form:
                    // 0 1 2 3 4 5 6 7
                    // | field |
                    // We only perform right shift.
                    write!(output, "{read_byte}>>{}", 7 - end.bit_pos).unwrap();
                } else if self.start.bit_pos > 0 {
                    // The field has the following form:
                    // 0 1 2 3 4 5 6 7
                    //       | field |
                    // We only perform bitwise and.
                    write!(output, "{read_byte}&{}", ones_mask(0, self.field.bit - 1)).unwrap();
                } else {
                    // The field has the following form:
                    // 0 1 2 3 4 5 6 7
                    // |     field   |
                    // We directly index the underlying byte.
                    write!(output, "{read_byte}").unwrap();
                }
            }
            BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
                // The field is stored over multiple bytes and will be read
                // as an integer type while honoring the network endianess.
                {
                    // Create a new writer that will prepend a method for reading a byte slice
                    // as an integer type while honoring endianess.
                    let mut new = network_endian_read(&mut output, self.field.bit);

                    // Fill in the byteslice that need to be read from.
                    write!(
                        new.get_writer(),
                        "&{target_slice}[{}..{}]",
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    )
                    .unwrap();
                }

                if end.bit_pos < 7 {
                    // The field has the form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    // |     field           |
                    // We perform a right shift.
                    write!(output, ">>{}", 7 - end.bit_pos).unwrap();
                } else if self.start.bit_pos > 0 {
                    // The field has the form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    //         |     field           |
                    // We perform a bitwise and.
                    write!(output, "&{}", ones_mask(0, self.field.bit - 1)).unwrap();
                } else {
                    // The field has the form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    // |      field                  |
                    // We just do nothing.
                }
            }
            _ => panic!(),
        }
    }

    // Generae a code piece that read a field if field's `repr` is `U8` and
    // the field crosses byte boundaries.
    fn read_field_cross_byte(&self, target_slice: &str, output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);

        // The field will have the following form:
        // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
        //       |     fie-ld    |
        // The field is splitted into two parts by the byte boundary:
        // The 1st part ("({}[{}]<<{})") is :
        // 0 1 2 3 4 5 6 7
        //       |  fie- |
        // We need to left a left-shift to make room for the 2nd part.
        // The 2nd part ("({}[{}]>>{})") is :
        // 0 1 2 3 4 5 6 7
        // |-ld|
        // The 2nd part should right-shift to 7th bit.
        // Finally, we glue the two parts together with bitwise or.
        let read_result = format!(
            "({target_slice}[{}]<<{})|({target_slice}[{}]>>{})",
            self.start.byte_pos,
            end.bit_pos + 1,
            end.byte_pos,
            7 - end.bit_pos
        );

        if self.field.bit < 8 {
            // Clear the extra bits if the field size is smaller than 8.
            write!(
                output,
                "({read_result})&{}",
                ones_mask(0, self.field.bit - 1)
            )
            .unwrap();
        } else {
            // Otherwise, read the field as it is.
            write!(output, "{read_result}").unwrap();
        }
    }

    // Generae a code piece that read the field from the `target_slice` into
    // a `repr`-typed value.
    //
    // Note: this is the top-level method that combines `read_field` and `read_field_cross_byte`.
    fn read_repr(&self, target_slice: &str, output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);
        if self.field.bit <= 8 && self.start.byte_pos != end.byte_pos {
            self.read_field_cross_byte(target_slice, output);
        } else {
            self.read_field(target_slice, output);
        }
    }

    // Generate a code piece that read the field from the `target_slice` into a
    // `arg`-typed value.
    //
    // Note: we first call `read_repr` to read the field into a `repr`-typed value.
    // Then we convert the `repr`-typed value into `arg`-typed one.
    fn read_as_arg(&self, target_slice: &str, mut output: &mut dyn Write) {
        match &self.field.arg {
            Arg::Code(code) => {
                // `arg` is a rust type.
                // We force a converter that turns the `repr`-typed value
                // into `arg`-typed one.
                let mut into_writer = HeadTailWriter::new(
                    &mut output,
                    &format!("{}(", to_rust_type(self.field.repr, code)),
                    ")",
                );
                // Read the `repr`-typed value.
                self.read_repr(target_slice, into_writer.get_writer());
            }
            Arg::BuiltinTypes(defined_arg) => {
                if *defined_arg == self.field.repr {
                    // `arg` is the same as the `repr`.
                    // Simply read the `repr`-typed value.
                    self.read_repr(target_slice, output);
                } else {
                    // `arg` is bool and field.bit == 1.
                    // We generate fast-path code for converting the single-bit
                    // field to bool type.
                    // The code has the form: "field_slice[field_index]&0x1 != 0",
                    // evaluting to `true` if the field bit is 1, and `false` otherwise.
                    write!(
                        output,
                        "{target_slice}[{}]&{} != 0",
                        self.start.byte_pos,
                        ones_mask(7 - self.start.bit_pos, 7 - self.start.bit_pos)
                    )
                    .unwrap();
                }
            }
        }
    }
}

struct LengthGetMethod<'a> {
    field: &'a Field,
    start: BitPos,
    expr: &'a UsableAlgExpr,
}

impl<'a> LengthGetMethod<'a> {
    // Generate a get method to access the length field with name
    // `length_field_name` from the buffer slice `target_slice`.
    // The generated method is written to `output`.
    fn code_gen(&self, length_field_name: &str, target_slice: &str, mut output: &mut dyn Write) {
        // Generate function definition for a length field get method.
        // It will generate:
        // pub fn length_field_name(&self) -> usize {
        // ...
        // }
        let func_def = format!("#[inline]\npub fn {length_field_name}(&self)->usize{{\n");
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Here, the checks performed by the parser will ensure that
        // field `arg` is the same as field `repr`, and that `repr` is
        // one of `U8`, `U16`, `U32` and `U64`.
        // We also ensure in the parser that the calculated length will
        // not exceed a pre-defined constant that is way smaller than the
        // maximum value of `USIZE`.
        // So, we first read the field value into a local variable `length_val`,
        // and cast its type to `usize`.
        {
            let mut temp_writer = HeadTailWriter::new(
                func_def_writer.get_writer(),
                "let length_val=(",
                ") as usize;\n",
            );
            FieldGetMethod {
                field: self.field,
                start: self.start,
            }
            .read_repr(target_slice, temp_writer.get_writer());
        }

        // The parser will make sure that evaluating the expression
        // will not lead to any overflow.
        self.expr
            .gen_exec("length_val", func_def_writer.get_writer());
    }
}

struct FieldSetMethod<'a> {
    field: &'a Field,
    start: BitPos,
}

impl<'a> FieldSetMethod<'a> {
    // Generate a set method to set an input value `write_value` to
    // the field area with name `field_name` on the byte slice `target_slice`.
    // The generated method is written to `output`.
    fn code_gen(
        &self,
        field_name: &str,
        target_slice: &str,
        write_value: &str,
        mut output: &mut dyn Write,
    ) {
        if self.field.gen {
            // Generate function definition for a field set method.
            // It will generate:
            // pub fn set_field_name(&mut self, write_value: FieldArgType) {
            // ...
            // }
            let func_def = format!(
                "#[inline]\npub fn set_{field_name}(&mut self, {write_value}:{}){{\n",
                self.field.arg.to_string()
            );
            let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

            // Fill in the function body for a field set method.
            self.write_as_arg(target_slice, write_value, func_def_writer.get_writer());
        }
    }

    // Generate a code piece that write `write_value` of type `repr`
    // to the field area on the byte slice `target_slice`.
    //
    // Generally, the byte slice containing the field has the following form:
    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
    // | rest bits | |field          |
    // The `write_value` has the form:
    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
    //               |write_value    |
    //
    // This method has the following steps (some steps can be omitted depending on
    // the actual form of the field):
    // 1. read the rest of the bits from the byte slice:
    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
    // | rest bits |
    // 2. combine the rest bits with the `write_value` into:
    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
    // | rest bits | |write_value    |
    // 3. write to the interested area on the `target_slice`.
    //
    // Note: this method does not handle the condition that the `repr` is `U8` and the
    // field crosses the byte boundary.
    //
    // Also note: the `write_value` only contains valid bits on the field area,
    // the rest of the bits are all zeroed out.
    fn write_field(&self, target_slice: &str, write_value: &str, mut output: &mut dyn Write) {
        match self.field.repr {
            BuiltinTypes::ByteSlice => {
                // The `repr` is a `ByteSlice`.
                // The field has the following form:
                // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                // |          field              |
                // The field area contains no extra bits,
                // we just write `write_value` to the field area.
                let mut field_writer = HeadTailWriter::new(
                    &mut output,
                    &format!(
                        "(&mut {target_slice}[{}..{}]).copy_from_slice(",
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    ),
                    ");",
                );
                write!(field_writer.get_writer(), "{}", write_value).unwrap();
            }
            BuiltinTypes::U8 => {
                // The `repr` is a `U8`.
                // The following assertion shows that the field locates within
                // a single byte and does not cross the byte boundaries.
                let end = self.start.next_pos(self.field.bit);
                assert!(self.start.byte_pos == end.byte_pos);

                // The write target is the byte containing the field.
                let write_target = format!("{target_slice}[{}]", self.start.byte_pos);

                if self.field.bit % 8 == 0 {
                    // The field has the following form:
                    // 0 1 2 3 4 5 6 7
                    // |     field   |
                    // We directly assign the `write_value` to the write target.
                    write!(output, "{write_target}={write_value};").unwrap();
                } else {
                    // The field area contains extra bits and we extract
                    // the rest of the bits through a mask.
                    let rest_of_bits = format!(
                        "({target_slice}[{}]&{})",
                        self.start.byte_pos,
                        zeros_mask(7 - end.bit_pos, 7 - self.start.bit_pos)
                    );

                    if end.bit_pos == 7 {
                        // The field has the following form:
                        // 0 1 2 3 4 5 6 7
                        //       | field |
                        // `write_value` has the same form as field.
                        // We glue `rest_of_bits` with `write_value` and write
                        // to the `write_target`.
                        write!(output, "{write_target}={rest_of_bits}|{write_value};").unwrap();
                    } else {
                        // The field has the following form:
                        // 0 1 2 3 4 5 6 7
                        // | field |
                        // We left shift the `write_value` to make room
                        // for the rest of the bits.
                        // Then we glue them together and write to the
                        // `write_target`.
                        write!(
                            output,
                            "{write_target}={rest_of_bits}|({write_value}<<{});",
                            7 - end.bit_pos
                        )
                        .unwrap();
                    }
                }
            }
            BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
                let end = self.start.next_pos(self.field.bit);
                // The following assertion ensures that the field is stored over
                // multiple bytes.
                assert!(self.start.bit_pos == 0 || end.bit_pos == 7);

                if self.field.bit % 8 == 0 {
                    // The field has the form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    // |   field                     |

                    // Create a new writer that will prepend a method for writing to a byte
                    // slice while honoring endianess.
                    let mut field_writer = network_endian_write(&mut output, self.field.bit);

                    // Create a mutable byte slice covering the field area.
                    write!(
                        field_writer.get_writer(),
                        "&mut {target_slice}[{}..{}],",
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    )
                    .unwrap();

                    // The field area contains no extra bits, so
                    // we directly write the `write_value` to the field area.
                    write!(field_writer.get_writer(), "{}", write_value).unwrap();
                } else {
                    // The field has the form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    // |   field       | | rest bits |

                    {
                        // First, read the rest of the bits into a variable.
                        let mut let_assign = HeadTailWriter::new(
                            &mut output,
                            &format!("let {REST_OF_FIELD}="),
                            ";\n",
                        );

                        if end.bit_pos == 7 {
                            // The field has the form:
                            // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                            // |rest bits| |   field         |
                            // We do the following steps to read the rest of the bits:
                            // 1. Read the byte containing the rest of the bits ("{}[{}]").
                            // 2. Remove the extra bits that belong to the field area ("{}[{}]&{}").
                            // 3. Convert the value to `repr` type ("({}[{}]&{}) as {})")
                            // 4. Left shift to make room for the field area ("(({}[{}]&{}) as {}) << {}")
                            write!(
                                let_assign.get_writer(),
                                "(({target_slice}[{}]&{}) as {}) << {}",
                                self.start.byte_pos,
                                ones_mask(8 - self.start.bit_pos, 7),
                                self.field.repr.to_string(),
                                8 * (byte_len(self.field.bit) - 1),
                            )
                            .unwrap();
                        } else {
                            // The field has the form:
                            // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                            // |   field         | |rest bits|
                            // We do similar steps except for the final one (the left-shift one).
                            write!(
                                let_assign.get_writer(),
                                "({target_slice}[{}]&{}) as {}",
                                end.byte_pos,
                                ones_mask(0, 6 - end.bit_pos),
                                self.field.repr.to_string()
                            )
                            .unwrap();
                        }
                    }

                    // Create a new writer that will prepend a method for writing an integer type
                    // to to a byte slice while honoring endianess.
                    let mut field_writer = network_endian_write(&mut output, self.field.bit);

                    // Specify the target slice to write to.
                    write!(
                        field_writer.get_writer(),
                        "&mut {target_slice}[{}..{}],",
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    )
                    .unwrap();

                    if end.bit_pos == 7 {
                        // The field has the following form:
                        // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                        //             |   field         |
                        // `write_value` has the same form as field.
                        // We glue the variable defined as `REST_OF_FIELD`
                        // and `write_value` together.
                        write!(field_writer.get_writer(), "{REST_OF_FIELD}|{write_value}",)
                            .unwrap();
                    } else {
                        // The field has the following form:
                        // 0 1 2 3 4 5 6 7
                        // | field |
                        // We left shift the `write_value` to make room
                        // for the rest of the bits.
                        // Then we glue them together.
                        write!(
                            field_writer.get_writer(),
                            "{REST_OF_FIELD}|({write_value}<<{})",
                            7 - end.bit_pos
                        )
                        .unwrap();
                    }
                }
            }
            _ => panic!(),
        }
    }

    // Generae a code piece that write a field if field's `repr` is `U8` and
    // the field crosses byte boundaries.
    fn write_field_cross_byte(
        &self,
        target_slice: &str,
        write_value: &str,
        output: &mut dyn Write,
    ) {
        let end = self.start.next_pos(self.field.bit);
        assert!(self.field.bit <= 8 && self.start.byte_pos != end.byte_pos);

        // The field will have the following form:
        // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
        //       |     fie-ld    |
        // The field is splitted into two parts by the byte boundary:

        // The 1st part is :
        // 0 1 2 3 4 5 6 7
        //       |  fie- |
        // To write to the 1st part, we do the following steps:
        // 1. Read the rest of the bits on the first part ("({}[{}]&{})")
        // 2. Right shift the `write_value` ("({}>>{})")
        // 3. Glue them together and write to the area covering the 1st part.
        let start_byte_pos = self.start.byte_pos;
        write!(
            output,
            "{target_slice}[{start_byte_pos}]=({target_slice}[{start_byte_pos}]&{})|({write_value}>>{});\n",
            zeros_mask(0, 7 - self.start.bit_pos),
            end.bit_pos + 1
        )
        .unwrap();

        // The 2nd part ("({}[{}]>>{})") is :
        // 0 1 2 3 4 5 6 7
        // |-ld|
        // To write to the 2nd part, we do the following steps:
        // 1. Read the rest of the bits on the 2nd part ("({}[{}]&{})")
        // 2. Left shift the `write_value` ("({}<<{})")
        // 3. Glue them together and write to the area covering the 2nd part.
        let end_byte_pos = end.byte_pos;
        write!(
            output,
            "{target_slice}[{end_byte_pos}]=({target_slice}[{end_byte_pos}]&{})|({write_value}<<{});",
            zeros_mask(7 - end.bit_pos, 7),
            7 - end.bit_pos
        )
        .unwrap();
    }

    // The top-level method for generating code piece that writes
    // a value to the field.
    //
    // Note: it combines `write_field` and `write_field_cross_byte`.
    fn write_repr(&self, target_slice: &str, write_value: &str, output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);
        if self.field.bit <= 8 && self.start.byte_pos != end.byte_pos {
            self.write_field_cross_byte(target_slice, write_value, output);
        } else {
            self.write_field(target_slice, write_value, output);
        }
    }

    // Generate a code piece for writing an input value `write_value` of type `arg` to the field
    // area stored on `target_slice`.
    fn write_as_arg(&self, target_slice: &str, write_value: &str, output: &mut dyn Write) {
        match &self.field.arg {
            Arg::BuiltinTypes(defined_arg) if *defined_arg != self.field.repr => {
                // Generate a fast path method in case that
                //`bit` is 1, `repr` is `U8` and `arg` is bool.
                // This will write 1 to the field bit if `write_value` is true,
                // and write 0 to the field bit if `write_value` is false.
                let start_byte_pos = self.start.byte_pos;
                write!(
                    output,
                    "if {write_value} {{
{target_slice}[{start_byte_pos}]={target_slice}[{start_byte_pos}]|{}
}} else {{
{target_slice}[{start_byte_pos}]={target_slice}[{start_byte_pos}]&{}
}}",
                    ones_mask(7 - self.start.bit_pos, 7 - self.start.bit_pos),
                    zeros_mask(7 - self.start.bit_pos, 7 - self.start.bit_pos)
                )
                .unwrap();
            }
            _ => {
                match &self.field.arg {
                    Arg::Code(_) => {
                        // `arg` is rust type.
                        // We convert the `write_value` to the `repr` type
                        // using `arg`'s compulsory association method.
                        write!(
                            output,
                            "let {write_value} = {};\n",
                            rust_var_as_repr(write_value, self.field.repr)
                        )
                        .unwrap();
                    }
                    _ => {}
                };
                if self.field.bit % 8 != 0 {
                    // The `write_value` will have the following form:
                    // 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
                    // |extra bits | |  write_value  |
                    // Here, we insert a guard condition to make sure that
                    // the extra bits on the `write_value` are all zeroed out.
                    write!(
                        output,
                        "assert!({write_value} <= {});\n",
                        ones_mask(0, self.field.bit - 1)
                    )
                    .unwrap();
                }
                self.write_repr(target_slice, write_value, output);
            }
        }
    }
}

struct LengthSetMethod<'a> {
    field: &'a Field,
    start: BitPos,
    expr: &'a UsableAlgExpr,
}

impl<'a> LengthSetMethod<'a> {
    // Generate a set method for the length field with name `length_field_name`.
    // The method will set the length value stored in `write_value` to the field
    // area stored in `target_slice`.
    // The generated method is written to `output`.
    fn code_gen(
        &self,
        length_field_name: &str,
        target_slice: &str,
        write_value: &str,
        mut output: &mut dyn Write,
    ) {
        // Generate function definition for a length field get method.
        // It will generate:
        // pub fn set_length_field_name(&mut self, write_value: usize) {
        // ...
        // }
        let func_def = format!(
            "#[inline]\npub fn set_{length_field_name}(&mut self, {write_value}:usize){{\n",
        );
        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");

        // Next, we calculate a series of guard conditions for the header set
        // method.
        let mut guards = Vec::new();

        /* Here, the `USIZE_BYTES` is the total byte number of the `usize` type.
        `usize` is the default argument type for all the length methods, this
        is designed to facilitate length calculation in rust.

        In the parser, we will make sure that, for the field that is used for
        length calculation, its bit size should not exceed `USIZE_BYTES * 8`.
        We will have two branches depending on the field bit size:

        1st, if field bit size equals `USIZE_BYTES * 8`, then the only possible
        way to calculate the maximum length without triggering overflow is to
        directly return the field value. In this case, we don't need any form of
        guard condition for the length set method.

        2nd, if the bit size is smaller than `USIZE_BYTES * 8`, we need to make sure that
        the input value to the header set method is smaller than the maximum expression value
        produced by the maximum field value. */
        if self.field.bit < crate::USIZE_BYTES * 8 {
            // This guard condition corresponds to the 2nd branch.
            guards.push(format!(
                "{write_value}<={}",
                self.expr
                    .exec((2 as u64).pow(self.field.bit as u32) - 1)
                    .unwrap()
            ));
        }
        let guard_str = self.expr.reverse_exec_guard(write_value);
        if guard_str.len() > 0 {
            // This guard condition ensures that the `write_value`
            // can be divided without remainder.
            guards.push(guard_str);
        }

        // If the guard conditions are present, we prepend them to the generated method.
        if guards.len() > 0 {
            let mut assert_writer =
                HeadTailWriter::new(func_def_writer.get_writer(), "assert!(", ");\n");
            guards.iter().enumerate().for_each(|(idx, s)| {
                write!(assert_writer.get_writer(), "({s})").unwrap();
                if idx < guards.len() - 1 {
                    write!(assert_writer.get_writer(), "&&").unwrap();
                }
            });
        }

        {
            // Perform a reverse calculation of the expression,
            // and assign the result to a new local variable.
            let mut val_def_writer = HeadTailWriter::new(
                func_def_writer.get_writer(),
                "let field_val = (",
                &format!(") as {};\n", self.field.repr.to_string()),
            );
            self.expr
                .gen_reverse_exec(write_value, val_def_writer.get_writer());
        }

        // Finally, set the new local variable containing the field value
        // to the field area on the `target_slice`.
        FieldSetMethod {
            field: self.field,
            start: self.start,
        }
        .write_repr(target_slice, "field_val", func_def_writer.get_writer());
    }
}

trait FieldAccessMethod {
    fn field_list(&self) -> &Vec<(String, Field)>;
    fn field_pos_map(&self) -> &HashMap<String, (BitPos, usize)>;
    fn length_fields(&self) -> &Vec<LengthField>;

    fn fixed_header_len(&self) -> u64 {
        let (field_name, field) = self.field_list().last().unwrap();
        let (start, _) = self.field_pos_map().get(field_name).unwrap();
        let end = start.next_pos(field.bit);

        end.byte_pos + 1
    }

    fn get_method_gen(&self, target_slice: &str, output: &mut dyn Write) {
        for (field_name, field) in self.field_list() {
            let (start, _) = self.field_pos_map().get(field_name).unwrap();

            FieldGetMethod {
                field,
                start: *start,
            }
            .code_gen(field_name, target_slice, output);
        }
    }

    fn set_method_gen(&self, target_slice: &str, write_value: &str, output: &mut dyn Write) {
        for (field_name, field) in self.field_list() {
            let (start, _) = self.field_pos_map().get(field_name).unwrap();

            FieldSetMethod {
                field,
                start: *start,
            }
            .code_gen(field_name, target_slice, write_value, output);
        }
    }

    fn get_length_gen(&self, target_slice: &str, output: &mut dyn Write) {
        self.length_fields()
            .iter()
            .enumerate()
            .for_each(|(idx, length_field)| {
                length_field.try_get_expr().map(|expr| {
                    let (start, field_idx) = self.field_pos_map().get(expr.field_name()).unwrap();
                    let (_, field) = &self.field_list()[*field_idx];

                    LengthGetMethod {
                        field,
                        start: *start,
                        expr,
                    }
                    .code_gen(LENGTH_FIELD_NAMES[idx], target_slice, output);
                });
            });
    }

    fn set_length_gen(&self, target_slice: &str, write_value: &str, output: &mut dyn Write) {
        self.length_fields()
            .iter()
            .enumerate()
            .for_each(|(idx, length_field)| {
                length_field.try_get_expr().map(|expr| {
                    let (start, field_idx) = self.field_pos_map().get(expr.field_name()).unwrap();
                    let (_, field) = &self.field_list()[*field_idx];

                    LengthSetMethod {
                        field,
                        start: *start,
                        expr,
                    }
                    .code_gen(
                        LENGTH_FIELD_NAMES[idx],
                        target_slice,
                        write_value,
                        output,
                    );
                });
            });
    }
}

macro_rules! impl_field_access_method {
    ($($ast_ty: ident),*) => {
        $(
            impl $crate::codegen::FieldAccessMethod for $ast_ty {
                fn field_list(&self) -> &::std::vec::Vec<(String, $crate::ast::Field)> {
                    &self.field_list
                }

                fn field_pos_map(
                    &self,
                ) -> &::std::collections::HashMap<String, ($crate::ast::BitPos, usize)> {
                    &self.field_pos_map
                }

                fn length_fields(&self) -> &::std::vec::Vec<$crate::ast::LengthField> {
                    &self.length_fields
                }
            }
        )*
    };
}
impl_field_access_method!(Packet, Message);

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
                .get_method_gen("self.buf.as_ref()", impl_block.get_writer());

            // Generate get methods for length fields if there are any.
            self.packet
                .get_length_gen("self.buf.as_ref()", impl_block.get_writer());
        }

        // Generate set methods
        {
            let mut impl_block = impl_block(
                "T:AsMut<[u8]>",
                &self.header_struct_name(),
                "T",
                &mut output,
            );

            // Generate set methods for various header fields.
            self.packet
                .set_method_gen("self.buf.as_mut()", "value", impl_block.get_writer());

            // Generate set methods for length fields if there are any.
            self.packet
                .set_length_gen("self.buf.as_mut()", "value", impl_block.get_writer());
        }
    }

    // Return the name of the header length const.
    fn header_len_name(&self) -> String {
        self.packet.protocol_name.to_uppercase() + "_HEADER_LEN"
    }

    // Return the name of the header struct.
    fn header_struct_name(&self) -> String {
        self.packet.protocol_name.clone() + "Header"
    }

    fn header_len_gen(&self, output: &mut dyn Write) {
        let header_len = self.packet.fixed_header_len();
        // TODO:
        // assert!(header_len < SOME_CONSTANT);

        write!(
            output,
            "/// A constant that defines the fixed byte length of the {} protocol header.
pub const {}: usize = {header_len};
",
            &self.packet.protocol_name,
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

pub struct PacketImpl<'a> {
    header_impl: &'a HeaderImpl<'a>,
}

impl<'a> PacketImpl<'a> {
    pub fn new(header_impl: &'a HeaderImpl<'a>) -> Self {
        Self { header_impl }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        // Generate packet struct definition.
        let packet_struct_gen = StructDefinition {
            struct_name: &self.packet_struct_name(),
            derives: &["Debug"],
        };
        packet_struct_gen.code_gen(output);

        {
            let mut impl_block = impl_block("T:Buf", &self.packet_struct_name(), "T", &mut output);

            // Generate `parse` and `parse_unchecked` methods.
            StructDefinition::parse_unchecked(impl_block.get_writer());

            // Generate the `buf`, `header` and `release` methods.
            StructDefinition::buf(impl_block.get_writer());
            self.header(impl_block.get_writer());
            StructDefinition::release(impl_block.get_writer());

            // Generate the get methods for various header fields.
            self.packet()
                .get_method_gen("self.buf.chunk()", impl_block.get_writer());

            // Generate the get methods for length fields if there are any.
            self.packet()
                .get_length_gen("self.buf.chunk()", impl_block.get_writer());

            // Generate the method that return the option slice if option presents.
            self.packet().option_name.as_ref().map(|option_name| {
                self.option(option_name, impl_block.get_writer());
            });
        }

        {
            let mut impl_block =
                impl_block("T:PktBuf", &self.packet_struct_name(), "T", &mut output);

            // Generate the `parse` and `payload` methods.
            self.parse(impl_block.get_writer());
            self.payload(impl_block.get_writer());
        }

        // Generate set methods
        {
            let mut impl_block =
                impl_block("T:BufMut", &self.packet_struct_name(), "T", &mut output);

            // Generate set methods for various header fields.
            self.packet()
                .set_method_gen("self.buf.chunk_mut()", "value", impl_block.get_writer());

            // Generate set methods for length fields if there are any.
            self.packet()
                .set_length_gen("self.buf.chunk_mut()", "value", impl_block.get_writer());

            // Generate the method that returns a mutable option slice if option presents.
            self.packet().option_name.as_ref().map(|option_name| {
                self.option_mut(option_name, impl_block.get_writer());
            });

            // Generate `prepend_header` method.
            self.prepend_header(impl_block.get_writer());
        }
    }

    // obtain a reference to the packet contained
    // in the `header_impl`
    fn packet(&self) -> &Packet {
        &self.header_impl.packet
    }

    fn packet_struct_name(&self) -> String {
        self.packet().protocol_name.clone() + "Packet"
    }

    fn header(&self, output: &mut dyn Write) {
        let header_struct_name = self.header_impl.header_struct_name();
        let header_len_name = self.header_impl.header_len_name();
        write!(
            output,
            "#[inline]
pub fn header(self) -> {header_struct_name}<&[u8]>{{
let data = &self.buf.chunk()[..{header_len_name}];
{header_struct_name}::parse_unchecked(data)
}}
"
        )
        .unwrap();
    }

    fn parse(&self, output: &mut dyn Write) {
        let packet_struct_name = self.packet_struct_name();
        let header_len_name = self.header_impl.header_len_name();

        // dump the start of the function body
        // we need to make sure that the length of the starting chunk is
        // larger than the fixed length of the packet header
        write!(
            output,
            "#[inline]
pub fn parse(buf: T) -> Result<{packet_struct_name}<T>, T> {{
let chunk_len = buf.chunk().len();
if chunk_len < {header_len_name} {{
return Err(buf);
}}
let packet = Self::parse_unchecked(buf);
"
        )
        .unwrap();

        // we will generate format check conditions in this array
        let mut guards = Vec::new();

        // if we have defined a header length expression, we need more checks to ensure that
        // the header length has the correct format
        let header_len_var = if self.packet().length_fields[HEADER_LEN_IDX].is_defined() {
            // the header_len of the packet must be larger than the fixed header length
            guards.push(format!("packet.header_len()<{header_len_name}"));
            guards.push(format!("packet.header_len()>chunk_len"));
            "packet.header_len()"
        } else {
            &header_len_name
        };

        if self.packet().length_fields[PAYLOAD_LEN_IDX].is_defined() {
            // add format check conditions for payload length,
            // make ensure that the packet fits within the buffer
            guards.push(format!(
                "packet.payload_len()+{header_len_var}>packet.buf.remaining()"
            ));
        } else if self.packet().length_fields()[PACKET_LEN_IDX].is_defined() {
            // add format check conditions for packet length,
            // make sure that the header fits within the packet,
            // and that the packet fits within the buffer
            guards.push(format!("packet.packet_len()<{header_len_var}"));
            guards.push(format!("packet.packet_len()>packet.buf.remaining()"));
        } else {
            // Do nothing
        }

        // we need to insert format checks depending on weather
        // the packet has variable length fields
        if guards.len() > 0 {
            let guard_str = if guards.len() == 1 {
                format!("{}", guards[0])
            } else {
                let mut buf = Vec::new();
                guards.iter().enumerate().for_each(|(idx, s)| {
                    write!(&mut buf, "({s})").unwrap();

                    if idx < guards.len() - 1 {
                        write!(&mut buf, "||").unwrap();
                    }
                });
                String::from_utf8(buf).unwrap()
            };

            write!(
                output,
                "if {guard_str} {{
return Err(packet.release());
}}
"
            )
            .unwrap();
        }

        // dirrectly return the buffer as it is, if the guarding conditions
        // are met.
        write!(output, "Ok(packet)\n}}\n").unwrap();
    }

    fn payload(&self, output: &mut dyn Write) {
        // If we have a variable packet length, we need to trim off
        // some bytes before we release the payload

        let header_len_name = self.header_impl.header_len_name();
        write!(
            output,
            "#[inline]
pub fn payload(self)->T{{
"
        )
        .unwrap();

        if self.packet().length_fields[PAYLOAD_LEN_IDX].is_defined() {
            // we have variable payload length
            let header_len_var = if self.packet().length_fields[HEADER_LEN_IDX].is_defined() {
                "self.header_len()"
            } else {
                &header_len_name
            };
            write!(
                output,
                "assert!({header_len_var}+self.payload_len()<=self.buf.remaining());
let trim_size = self.buf.remaining()-{header_len_var}-self.payload_len();
"
            )
            .unwrap();
        } else if self.packet().length_fields[PACKET_LEN_IDX].is_defined() {
            // we have variable packet length
            write!(
                output,
                "assert!(self.packet_len()<=self.buf.remaining());
let trim_size = self.buf.remaining()-self.packet_len();
"
            )
            .unwrap();
        } else {
            // Do nothing
        }

        let header_len_var = if self.packet().length_fields[HEADER_LEN_IDX].is_defined() {
            // Here, we have variable header length, so we must save the length
            // to a local variable before we release the buffer
            write!(output, "let header_len = self.header_len();\n").unwrap();
            "header_len"
        } else {
            // The header has fixed length, we use the pre-defined constant
            &header_len_name
        };

        // release the internal buffer from the packet
        write!(output, "let mut buf = self.release();\n").unwrap();
        if self.packet().length_fields[PAYLOAD_LEN_IDX].is_defined()
            || self.packet().length_fields[PACKET_LEN_IDX].is_defined()
        {
            // if we have variable packet length, we are gona need to trim off the trailing bytes
            // beyond the end of the packet
            write!(
                output,
                "if trim_size > 0 {{
buf.trim_off(trim_size);
}}
"
            )
            .unwrap();
        }

        // advance the cursor beyond the header
        write!(
            output,
            "buf.advance({header_len_var});
buf
}}
"
        )
        .unwrap();
    }

    fn prepend_header(&self, output: &mut dyn Write) {
        let packet_struct_name = self.packet_struct_name();
        let header_len_name = self.header_impl.header_len_name();
        write!(
            output,
            "#[inline]
pub fn prepend_header<HT: AsRef<[u8]>>(mut buf: T, header: &{}<HT>) -> {packet_struct_name}<T> {{
",
            self.header_impl.header_struct_name()
        )
        .unwrap();

        // if we have variable payload length, we save it to a local variable
        if self.packet().length_fields[PAYLOAD_LEN_IDX].is_defined() {
            write!(output, "let payload_len = buf.remaining();\n").unwrap();
        }

        // here, we record the name of the header length variable
        let header_len_var = if self.packet().length_fields[HEADER_LEN_IDX].is_defined() {
            // add a guard condition to make sure that the header_len is larger
            // than the fixed header length
            write!(output, "assert!(header.header_len()>={header_len_name});\n",).unwrap();
            "header.header_len()"
        } else {
            &header_len_name
        };

        // move the cursor back and copy the packet content in
        write!(
            output,
            "buf.move_back({header_len_var});
(&mut buf.chunk_mut()[0..{header_len_name}]).copy_from_slice(header.as_bytes());
"
        )
        .unwrap();

        if !self.packet().length_fields[PAYLOAD_LEN_IDX].is_defined()
            && !self.packet().length_fields[PACKET_LEN_IDX].is_defined()
        {
            // if we do not have variable payload length or packet length,
            // we can construct a packet variable and return it
            write!(output, "{packet_struct_name}::parse_unchecked(buf)\n",).unwrap();
        } else {
            let adjust_packet_length = if self.packet().length_fields[PAYLOAD_LEN_IDX].is_defined()
            {
                "pkt.set_payload_len(payload_len);"
            } else if self.packet().length_fields[PACKET_LEN_IDX].is_defined() {
                "pkt.set_packet_len(pkt.buf.remaining());"
            } else {
                panic!()
            };

            // create a mutable packet variable
            write!(
                output,
                "let mut pkt = {packet_struct_name}::parse_unchecked(buf);
{adjust_packet_length}
pkt
}}
",
            )
            .unwrap();
        }

        write!(output, "}}\n").unwrap();
    }

    fn option(&self, option_name: &str, output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn {option_name}(&self)->&[u8]{{
&self.buf.chunk()[{}..self.header_len()]
}}
",
            self.header_impl.header_len_name()
        )
        .unwrap();
    }

    fn option_mut(&self, option_name: &str, output: &mut dyn Write) {
        write!(
            output,
            "#[inline]
pub fn {option_name}_mut(&mut self)->&mut [u8]{{
&mut self.buf.chunk_mut()[{}..self.header_len()]
}}
",
            self.header_impl.header_len_name()
        )
        .unwrap();
    }
}

pub struct MessageImpl<'a> {
    message: &'a Message,
}

impl<'a> MessageImpl<'a> {
    pub fn new(message: &'a Message) -> Self {
        Self { message }
    }

    // pub fn code_gen(&self, mut output: &mut dyn Write) {
    //     // Generate message struct definition.
    //     let message_struct_gen = StructDefinition {
    //         struct_name: &self.message_struct_name(),
    //         derives: &["Debug"],
    //     };
    //     message_struct_gen.code_gen(output);
    //     writeln!(output).unwrap();

    //     // Generate packet base methods.
    //     {
    //         let mut impl_block = impl_block(
    //             "T:AsRef<[u8]>",
    //             &self.message_struct_name(),
    //             "T",
    //             &mut output,
    //         );
    //         parse_unchecked(impl_block.get_writer());
    //         // self.header_impl.parse_unchecked(impl_block.get_writer());
    //         // self.buf_method(impl_block.get_writer());
    //         // self.release_method(impl_block.get_writer());
    //         // self.header_method(impl_block.get_writer());

    //         writeln!(impl_block.get_writer()).unwrap();
    //     }
    // }

    fn message_struct_name(&self) -> String {
        format!("{}Message", self.message.protocol_name)
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
        ($program: expr, $test_ty: ident, $test_fn: ident, $expected: expr, $start: expr $(, $arg: expr)*) => {
            let tokenizer = Tokenizer::new($program);
            let field = parse_with_error!(crate::parser::FieldParser, tokenizer).unwrap();
            let mut buf: Vec<u8> = ::std::vec::Vec::new();
            $test_ty{field: &field, start: $start}.$test_fn($($arg,)* &mut buf);
            assert_eq!($expected, std::str::from_utf8(&buf[..]).unwrap());
        };
    }

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
            "Field {bit  = 32, repr = &[u8], arg = %%Ipv4Addr%%, default=%%Ipv4Addr::default()%%}",
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
            "Field {bit  = 32, repr = &[u8], arg = %%Ipv4Addr%%, default=%%Ipv4Addr::default()%%}",
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
