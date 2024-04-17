use std::collections::HashMap;
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
    let byte_len = byte_len(bit_len);
    match byte_len {
        2 => HeadTailWriter::new(writer, "NetworkEndian::write_u16(", ");"),
        3 => HeadTailWriter::new(writer, "NetworkEndian::write_u24(", ");"),
        4 => HeadTailWriter::new(writer, "NetworkEndian::write_u32(", ");"),
        5 | 6 | 7 => HeadTailWriter::new(
            writer,
            "NetworkEndian::write_uint(",
            &format!(",{});", byte_len),
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

fn to_rust_type(repr: BuiltinTypes, rust_type_code: &str) -> String {
    match repr {
        BuiltinTypes::U8 | BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
            format!("{}::from_{}", rust_type_code, repr.to_string())
        }
        BuiltinTypes::ByteSlice => {
            format!("{}::from_byte_slice", rust_type_code)
        }
        _ => panic!(),
    }
}

fn rust_var_as_repr(var_name: &str, repr: BuiltinTypes) -> String {
    match repr {
        BuiltinTypes::U8 | BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
            format!("{}.as_{}()", var_name, repr.to_string())
        }
        BuiltinTypes::ByteSlice => {
            format!("{}.as_byte_slice()", var_name)
        }
        _ => panic!(),
    }
}

struct FieldGetMethod<'a> {
    field: &'a Field,
    start: BitPos,
}

impl<'a> FieldGetMethod<'a> {
    fn code_gen(&self, field_name: &str, target_slice: &str, mut output: &mut dyn Write) {
        writeln!(output, "#[inline]").unwrap();

        let mut func_def = if self.field.gen {
            "pub fn ".to_string()
        } else {
            "fn ".to_string()
        };
        func_def += &format!("{}(&self)->{}{{\n", field_name, self.field.arg.to_string());

        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");
        self.read_as_arg(target_slice, func_def_writer.get_writer());
    }

    fn read_field(&self, target_slice: &str, mut output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);
        match self.field.repr {
            BuiltinTypes::ByteSlice => {
                write!(
                    output,
                    "&{}[{}..{}]",
                    target_slice,
                    self.start.byte_pos,
                    self.start.byte_pos + byte_len(self.field.bit)
                )
                .unwrap();
            }
            BuiltinTypes::U8 => {
                assert!(self.start.byte_pos == end.byte_pos);

                let read_byte = format!("{}[{}]", target_slice, self.start.byte_pos);

                if end.bit_pos < 7 && self.start.bit_pos > 0 {
                    // right shift followed by bitwise and
                    write!(
                        output,
                        "({}>>{})&{}",
                        read_byte,
                        7 - end.bit_pos,
                        ones_mask(0, self.field.bit - 1)
                    )
                    .unwrap();
                } else if end.bit_pos < 7 {
                    // right shift only
                    write!(output, "{}>>{}", read_byte, 7 - end.bit_pos).unwrap();
                } else if self.start.bit_pos > 0 {
                    // bitwise and only
                    write!(output, "{}&{}", read_byte, ones_mask(0, self.field.bit - 1)).unwrap();
                } else {
                    // field.bit % 8 == 0, do nothing
                    write!(output, "{}", read_byte).unwrap();
                }
            }
            BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
                {
                    let mut new = network_endian_read(&mut output, self.field.bit);
                    write!(
                        new.get_writer(),
                        "&{}[{}..{}]",
                        target_slice,
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    )
                    .unwrap();
                }

                if end.bit_pos < 7 {
                    write!(output, ">>{}", 7 - end.bit_pos).unwrap();
                } else if self.start.bit_pos > 0 {
                    write!(output, "&{}", ones_mask(0, self.field.bit - 1)).unwrap();
                } else {
                    // field.bit % 8 == 0, do nothing
                }
            }
            _ => panic!(),
        }
    }

    fn read_field_cross_byte(&self, target_slice: &str, output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);

        let read_result = format!(
            "({}[{}]<<{})|({}[{}]>>{})",
            target_slice,
            self.start.byte_pos,
            self.field.bit - (8 - self.start.bit_pos),
            target_slice,
            end.byte_pos,
            7 - end.bit_pos
        );

        if self.field.bit < 8 {
            write!(
                output,
                "({})&{}",
                read_result,
                ones_mask(0, self.field.bit - 1)
            )
            .unwrap();
        } else {
            write!(output, "{}", read_result).unwrap();
        }
    }

    fn read_repr(&self, target_slice: &str, output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);
        if self.field.bit <= 8 && self.start.byte_pos != end.byte_pos {
            self.read_field_cross_byte(target_slice, output);
        } else {
            self.read_field(target_slice, output);
        }
    }

    fn read_as_arg(&self, target_slice: &str, mut output: &mut dyn Write) {
        match &self.field.arg {
            Arg::Code(code) => {
                // arg is code
                let mut into_writer = HeadTailWriter::new(
                    &mut output,
                    &format!("{}(", to_rust_type(self.field.repr, code)),
                    ")",
                );
                self.read_repr(target_slice, into_writer.get_writer());
            }
            Arg::BuiltinTypes(defined_arg) => {
                if *defined_arg == self.field.repr {
                    // arg is the same as the repr
                    self.read_repr(target_slice, output);
                } else {
                    // arg is bool, field.bit == 1
                    // A fast path method for accessing bool
                    write!(
                        output,
                        "{}[{}]&{} != 0",
                        target_slice,
                        self.start.byte_pos,
                        ones_mask(7 - self.start.bit_pos, 7 - self.start.bit_pos)
                    )
                    .unwrap();
                }
            }
        }
    }
}

struct FieldSetMethod<'a> {
    field: &'a Field,
    start: BitPos,
}

impl<'a> FieldSetMethod<'a> {
    fn code_gen(
        &self,
        field_name: &str,
        target_slice: &str,
        write_value: &str,
        mut output: &mut dyn Write,
    ) {
        writeln!(output, "#[inline]").unwrap();

        let mut func_def = if self.field.gen {
            "pub fn ".to_string()
        } else {
            "fn ".to_string()
        };
        func_def += &format!(
            "set_{}(&mut self, {}:{}){{\n",
            field_name,
            write_value,
            self.field.arg.to_string()
        );

        let mut func_def_writer = HeadTailWriter::new(&mut output, &func_def, "\n}\n");
        self.write_as_arg(target_slice, write_value, func_def_writer.get_writer());
    }

    fn write_field(&self, target_slice: &str, write_value: &str, mut output: &mut dyn Write) {
        // we first check some assertions, they are only
        // added here to help me reason about the algorithm
        match self.field.repr {
            BuiltinTypes::ByteSlice => {
                assert!(self.field.bit % 8 == 0 && self.field.bit > 8);
                let mut field_writer = HeadTailWriter::new(
                    &mut output,
                    &format!(
                        "(&mut {}[{}..{}]).copy_from_slice(",
                        target_slice,
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    ),
                    ");",
                );
                write!(field_writer.get_writer(), "{}", write_value).unwrap();
            }
            // we do not handle the cross byte u8
            BuiltinTypes::U8 => {
                let end = self.start.next_pos(self.field.bit);
                assert!(self.start.byte_pos == end.byte_pos);

                let write_target = format!("{}[{}]", target_slice, self.start.byte_pos);

                if self.field.bit % 8 == 0 {
                    // the field is located in a single byte
                    write!(output, "{}={};", write_target, write_value).unwrap();
                } else {
                    let rest_of_field = format!(
                        "({}[{}]&{})",
                        target_slice,
                        self.start.byte_pos,
                        zeros_mask(7 - end.bit_pos, 7 - self.start.bit_pos)
                    );

                    if end.bit_pos == 7 {
                        // no left shift
                        write!(
                            output,
                            "{}={}|{};",
                            write_target, rest_of_field, write_value
                        )
                        .unwrap();
                    } else {
                        // left shift
                        write!(
                            output,
                            "{}={}|({}<<{});",
                            write_target,
                            rest_of_field,
                            write_value,
                            7 - end.bit_pos
                        )
                        .unwrap();
                    }
                }
            }
            BuiltinTypes::U16 | BuiltinTypes::U32 | BuiltinTypes::U64 => {
                let end = self.start.next_pos(self.field.bit);
                assert!(self.start.bit_pos == 0 || end.bit_pos == 7);

                if self.field.bit % 8 == 0 {
                    let mut field_writer = network_endian_write(&mut output, self.field.bit);
                    write!(
                        field_writer.get_writer(),
                        "&mut {}[{}..{}],",
                        target_slice,
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    )
                    .unwrap();
                    write!(field_writer.get_writer(), "{}", write_value).unwrap();
                } else {
                    // first, read rest of the field into a temporary variable REST_OF_FIELD
                    {
                        let mut let_assign = HeadTailWriter::new(
                            &mut output,
                            &format!("let {}=", REST_OF_FIELD),
                            ";\n",
                        );
                        if end.bit_pos == 7 {
                            write!(
                                let_assign.get_writer(),
                                "(({}[{}]&{}) as {}) << {}",
                                target_slice,
                                self.start.byte_pos,
                                ones_mask(8 - self.start.bit_pos, 7),
                                self.field.repr.to_string(),
                                8 * (byte_len(self.field.bit) - 1),
                            )
                            .unwrap();
                        } else {
                            write!(
                                let_assign.get_writer(),
                                "({}[{}]&{}) as {}",
                                target_slice,
                                end.byte_pos,
                                ones_mask(0, 6 - end.bit_pos),
                                self.field.repr.to_string()
                            )
                            .unwrap();
                        }
                    }

                    // setup the write guard
                    let mut field_writer = network_endian_write(&mut output, self.field.bit);
                    // specify the target slice to write to
                    write!(
                        field_writer.get_writer(),
                        "&mut {}[{}..{}],",
                        target_slice,
                        self.start.byte_pos,
                        self.start.byte_pos + byte_len(self.field.bit)
                    )
                    .unwrap();

                    if end.bit_pos == 7 {
                        // no left shift
                        write!(
                            field_writer.get_writer(),
                            "{}|{}",
                            REST_OF_FIELD,
                            write_value
                        )
                        .unwrap();
                    } else {
                        // left shift
                        write!(
                            field_writer.get_writer(),
                            "{}|({}<<{})",
                            REST_OF_FIELD,
                            write_value,
                            7 - end.bit_pos
                        )
                        .unwrap();
                    }
                }
            }
            _ => panic!(),
        }
    }

    fn write_field_cross_byte(
        &self,
        target_slice: &str,
        write_value: &str,
        output: &mut dyn Write,
    ) {
        let end = self.start.next_pos(self.field.bit);
        assert!(self.field.bit <= 8 && self.start.byte_pos != end.byte_pos);

        write!(
            output,
            "{}[{}]=({}[{}]&{})|({}>>{});\n",
            target_slice,
            self.start.byte_pos,
            target_slice,
            self.start.byte_pos,
            zeros_mask(0, 7 - self.start.bit_pos),
            write_value,
            end.bit_pos + 1
        )
        .unwrap();

        write!(
            output,
            "{}[{}]=({}[{}]&{})|({}<<{});",
            target_slice,
            end.byte_pos,
            target_slice,
            end.byte_pos,
            zeros_mask(7 - end.bit_pos, 7),
            write_value,
            7 - end.bit_pos
        )
        .unwrap();
    }

    fn write_repr(&self, target_slice: &str, write_value: &str, output: &mut dyn Write) {
        let end = self.start.next_pos(self.field.bit);
        if self.field.bit <= 8 && self.start.byte_pos != end.byte_pos {
            self.write_field_cross_byte(target_slice, write_value, output);
        } else {
            self.write_field(target_slice, write_value, output);
        }
    }

    fn write_as_arg(&self, target_slice: &str, write_value: &str, output: &mut dyn Write) {
        match &self.field.arg {
            Arg::BuiltinTypes(defined_arg) if *defined_arg != self.field.repr => {
                write!(output, "if {} {{\n", write_value).unwrap();
                write!(
                    output,
                    "{}[{}]={}[{}]|{}\n",
                    target_slice,
                    self.start.byte_pos,
                    target_slice,
                    self.start.byte_pos,
                    ones_mask(7 - self.start.bit_pos, 7 - self.start.bit_pos)
                )
                .unwrap();
                write!(output, "}}\nelse {{\n").unwrap();
                write!(
                    output,
                    "{}[{}]={}[{}]&{}\n",
                    target_slice,
                    self.start.byte_pos,
                    target_slice,
                    self.start.byte_pos,
                    zeros_mask(7 - self.start.bit_pos, 7 - self.start.bit_pos)
                )
                .unwrap();
                write!(output, "}}").unwrap();
            }
            _ => {
                match &self.field.arg {
                    Arg::Code(_) => {
                        write!(
                            output,
                            "let {} = {};\n",
                            write_value,
                            rust_var_as_repr(write_value, self.field.repr)
                        )
                        .unwrap();
                    }
                    _ => {}
                };
                if self.field.bit % 8 != 0 {
                    write!(
                        output,
                        "assert!({} <= {});\n",
                        write_value,
                        ones_mask(0, self.field.bit - 1)
                    )
                    .unwrap();
                }
                self.write_repr(target_slice, write_value, output);
            }
        }
    }
}

pub struct FieldAccessMethodBlob<'a> {
    field_list: &'a Vec<(String, Field)>,
    field_pos_map: &'a HashMap<String, (BitPos, usize)>,
    type_name: &'a str,
    trait_name: &'a str,
    target_slice: &'a str,
    write_value: Option<&'a str>,
}

impl<'a> FieldAccessMethodBlob<'a> {
    pub fn new(
        field_list: &'a Vec<(String, Field)>,
        field_pos_map: &'a HashMap<String, (BitPos, usize)>,
        type_name: &'a str,
        trait_name: &'a str,
        target_slice: &'a str,
        write_value: Option<&'a str>,
    ) -> Self {
        Self {
            field_list,
            field_pos_map,
            type_name,
            trait_name,
            target_slice,
            write_value,
        }
    }

    pub fn code_gen(&self, output: &mut dyn Write) {
        write!(
            output,
            "impl<T: {}> {}<T>{{\n",
            self.trait_name, self.type_name
        )
        .unwrap();

        for (field_name, field) in self.field_list {
            let (start, _) = self.field_pos_map.get(field_name).unwrap();
            match &self.write_value {
                Some(write_value) => {
                    FieldSetMethod {
                        field,
                        start: *start,
                    }
                    .code_gen(
                        field_name,
                        &self.target_slice,
                        write_value,
                        output,
                    );
                }
                None => {
                    FieldGetMethod {
                        field,
                        start: *start,
                    }
                    .code_gen(field_name, &self.target_slice, output);
                }
            }
        }

        write!(output, "}}\n").unwrap();
    }
}

struct StructDefinition<'a> {
    protocol_name: &'a str,
    suffix: &'a str,
    derives: &'a [&'static str],
}

impl<'a> StructDefinition<'a> {
    fn struct_name(&self) -> String {
        self.protocol_name.to_string() + self.suffix
    }

    fn code_gen(&self, mut output: &mut dyn Write) {
        assert!(self.derives.len() > 0);
        {
            let mut derive_writer = HeadTailWriter::new(&mut output, "#[derive(", ")]\n");
            self.derives
                .iter()
                .enumerate()
                .for_each(|(idx, derive_name)| {
                    write!(derive_writer.get_writer(), "{}", derive_name).unwrap();
                    if idx < self.derives.len() - 1 {
                        write!(derive_writer.get_writer(), ",").unwrap();
                    }
                });
        }
        write!(
            output,
            "pub struct {}{}<T> {{\n",
            self.protocol_name, self.suffix
        )
        .unwrap();
        write!(output, "buf: T\n").unwrap();
        write!(output, "}}\n").unwrap();
    }
}

struct HeaderLenConst<'a> {
    protocol_name: &'a str,
    field_list: &'a Vec<(String, Field)>,
    field_pos_map: &'a HashMap<String, (BitPos, usize)>,
}

impl<'a> HeaderLenConst<'a> {
    fn header_len_name(&self) -> String {
        self.protocol_name.to_uppercase() + "_HEADER_LEN"
    }

    fn code_gen(&self, output: &mut dyn Write) {
        write!(
            output,
            "/// A constant that defines the fixed byte length of the {} protocol header.\n",
            self.protocol_name
        )
        .unwrap();

        let (field_name, field) = self.field_list.last().unwrap();
        let (start, _) = self.field_pos_map.get(field_name).unwrap();
        let end = start.next_pos(field.bit);
        // Prevent an over-large header length.
        assert!(end.byte_pos < u64::MAX / 2);
        write!(
            output,
            "pub const {}: usize = {};\n",
            self.header_len_name(),
            end.byte_pos + 1
        )
        .unwrap();
    }
}

struct HeaderBaseMethod<'a> {
    header_struct_name: &'a str,
    header_len_name: &'a str,
}

impl<'a> HeaderBaseMethod<'a> {
    fn code_gen(&self, output: &mut dyn Write) {
        let trait_name = "AsRef<[u8]>";

        write!(
            output,
            "impl<T: {}> {}<T>{{\n",
            trait_name, self.header_struct_name
        )
        .unwrap();

        self.parse(output);
        self.parse_unchecked(output);
        self.as_byte_slice(output);
        self.to_owned(output);

        write!(output, "}}\n").unwrap();
    }

    fn parse(&self, output: &mut dyn Write) {
        write!(output, "#[inline]\n").unwrap();
        write!(output, "pub fn parse(buf: T) -> Result<Self, T>{{\n").unwrap();
        write!(
            output,
            "if buf.as_ref().len()>={}{{\n",
            self.header_len_name
        )
        .unwrap();
        write!(output, "Ok(Self{{buf}})\n").unwrap();
        write!(output, "}} else {{\n").unwrap();
        write!(output, "Err(buf)\n").unwrap();
        write!(output, "}}\n}}\n").unwrap();
    }

    fn parse_unchecked(&self, output: &mut dyn Write) {
        write!(output, "#[inline]\n").unwrap();
        write!(output, "pub fn parse_unchecked(buf: T) -> Self{{\n").unwrap();
        write!(output, "Self{{buf}}\n").unwrap();
        write!(output, "}}\n").unwrap();
    }

    fn as_byte_slice(&self, output: &mut dyn Write) {
        write!(output, "#[inline]\n").unwrap();
        write!(output, "pub fn as_byte_slice(&self) -> &[u8]{{\n").unwrap();
        write!(
            output,
            "&{}[0..{}]\n",
            "self.buf.as_ref()", self.header_len_name
        )
        .unwrap();
        write!(output, "}}\n").unwrap();
    }

    fn to_owned(&self, output: &mut dyn Write) {
        write!(output, "#[inline]\n").unwrap();
        write!(
            output,
            "pub fn to_owned(&self) -> {}<[u8; {}]>{{\n",
            self.header_struct_name, self.header_len_name
        )
        .unwrap();
        write!(output, "let mut buf = [0; {}];\n", self.header_len_name).unwrap();
        write!(output, "buf.copy_from_slice(self.as_bytes());\n").unwrap();
        write!(output, "{} {{buf}}\n", self.header_struct_name).unwrap();
        write!(output, "}}\n").unwrap();
    }
}

pub struct Header<'a> {
    pub packet: &'a Packet,
}

impl<'a> Header<'a> {
    pub fn code_gen(&self, output: &mut dyn Write) {
        // Generate a constant value that contains header length.
        let header_len_gen = HeaderLenConst {
            protocol_name: &self.packet.protocol_name,
            field_list: &self.packet.field_list,
            field_pos_map: &self.packet.field_pos_map,
        };
        header_len_gen.code_gen(output);
        writeln!(output).unwrap();

        // Defines the header struct.
        let header_struct_gen = StructDefinition {
            protocol_name: &self.packet.protocol_name,
            suffix: "Header",
            derives: &["Debug", "Clone", "Copy"],
        };
        header_struct_gen.code_gen(output);
        writeln!(output).unwrap();

        // Generate basic methods.
        let header_base_method_gen = HeaderBaseMethod {
            header_struct_name: &header_struct_gen.struct_name(),
            header_len_name: &header_len_gen.header_len_name(),
        };
        header_base_method_gen.code_gen(output);
        writeln!(output).unwrap();

        // Generate get methods
        let header_get_methods_gen = FieldAccessMethodBlob {
            field_list: &self.packet.field_list,
            field_pos_map: &self.packet.field_pos_map,
            type_name: &header_struct_gen.struct_name(),
            trait_name: "AsRef<[u8]>",
            target_slice: "self.buf.as_ref()",
            write_value: None,
        };
        header_get_methods_gen.code_gen(output);
        writeln!(output).unwrap();

        // Generate set methods
        let header_set_methods_gen = FieldAccessMethodBlob {
            field_list: &self.packet.field_list,
            field_pos_map: &self.packet.field_pos_map,
            type_name: &header_struct_gen.struct_name(),
            trait_name: "AsMut<[u8]>",
            target_slice: "self.buf.as_mut()",
            write_value: Some("value"),
        };
        header_set_methods_gen.code_gen(output);
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
}
else {
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
