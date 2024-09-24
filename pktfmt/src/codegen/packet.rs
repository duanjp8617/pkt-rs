use std::{default, io::Write};

use crate::ast::{DefaultVal, LengthField, Packet};

use super::{GenerateFieldAccessMethod, HeadTailWriter};
/// Packet type generator.
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
            let mut impl_block =
                impl_block("T:PktBuf", &self.packet_struct_name(), "T", &mut output);

            // Generate the `parse_unchecked` methods.
            StructDefinition::parse_unchecked(impl_block.get_writer());

            // Generate the `buf`, `header` and `release` methods.
            StructDefinition::buf(impl_block.get_writer());
            self.header(impl_block.get_writer());
            StructDefinition::release(impl_block.get_writer());

            // Generate the get methods for various header fields.
            self.packet().header_field_method_gen(
                "self.buf.chunk()",
                None,
                impl_block.get_writer(),
            );

            // Generate the get methods for length fields if there are any.
            self.packet().length_field_method_gen(
                "self.buf.chunk()",
                None,
                impl_block.get_writer(),
            );

            // Generate the `parse` and `payload` methods.
            self.parse(impl_block.get_writer());
            self.payload(impl_block.get_writer());
        }

        // Generate set methods
        {
            let mut impl_block =
                impl_block("T:BufMut", &self.packet_struct_name(), "T", &mut output);

            // Generate set methods for various header fields.
            self.packet().header_field_method_gen(
                "self.buf.chunk_mut()",
                Some("value"),
                impl_block.get_writer(),
            );

            // Generate set methods for length fields if there are any.
            self.packet().header_field_method_gen(
                "self.buf.chunk_mut()",
                Some("value"),
                impl_block.get_writer(),
            );

            // Generate `prepend_header` method.
            self.prepend_header(impl_block.get_writer());
        }
    }

    // A generator for the parse method.
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

        // if we have defined a header length expression, we need more checks to ensure
        // that the header length has the correct format
        let header_len_var = match self.packet().length().at(0) {
            LengthField::None => {
                // Header field is not defined, use the fixed header length.
                &header_len_name
            }
            LengthField::Undefined => {
                // Header field is defined without computing expression.
                // The return value of the user defined `header_len` method must convert to
                // `usize` safely.
                guards.push(format!("(packet.header_len() as usize)<{header_len_name}"));
                guards.push(format!("(packet.header_len() as usize)>chunk_len"));
                "(packet.header_len() as usize)"
            }
            LengthField::Expr { expr } => {
                // Header field is defined with computing expression.
                let (field, _) = self.packet().header().field(expr.field_name()).unwrap();
                if field.default_fix {
                    // The default is fixed, check against the fixed value.
                    let default_val = match field.default {
                        DefaultVal::Num(n) => n,
                        _ => panic!(),
                    };
                    let fixed_header_len = expr.exec(default_val).unwrap();
                    guards.push(format!(
                        "(packet.header_len() as usize)!={fixed_header_len}"
                    ));
                } else {
                    // The default is not fixed, check with the fixed header length.
                    guards.push(format!("(packet.header_len() as usize)<{header_len_name}"));
                }
                guards.push(format!("(packet.header_len() as usize)>chunk_len"));
                "(packet.header_len() as usize)"
            }
        };

        if self.packet().length().at(1).appear() {
            // add format check conditions for payload length,
            // make ensure that the packet fits within the buffer
            guards.push(format!(
                "(packet.payload_len() as usize)+{header_len_var}>packet.buf.remaining()"
            ));
        } else if self.packet().length().at(2).appear() {
            // add format check conditions for packet length,
            // make sure that the header fits within the packet,
            // and that the packet fits within the buffer
            guards.push(format!("(packet.packet_len() as usize)<{header_len_var}"));
            guards.push(format!(
                "(packet.packet_len() as usize)>packet.buf.remaining()"
            ));
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

        if self.packet().length().at(1).appear() {
            // we have variable payload length
            let header_len_var = if self.packet().length().at(0).appear() {
                "(self.header_len() as usize)"
            } else {
                &header_len_name
            };
            write!(
                output,
                "assert!({header_len_var}+self.payload_len()<=self.buf.remaining());
let trim_size = self.buf.remaining()-({header_len_var}+self.payload_len());
"
            )
            .unwrap();
        } else if self.packet().length().at(2).appear() {
            // we have variable packet length
            write!(
                output,
                "assert!((self.packet_len() as usize)<=self.buf.remaining());
let trim_size = self.buf.remaining()-self.packet_len();
"
            )
            .unwrap();
        } else {
            // Do nothing
        }

        let header_len_var = if self.packet().length().at(0).appear() {
            // Here, we have variable header length, so we must save the length
            // to a local variable before we release the buffer
            write!(output, "let header_len = self.header_len() as usize;\n").unwrap();
            "header_len"
        } else {
            // The header has fixed length, we use the pre-defined constant
            &header_len_name
        };

        // release the internal buffer from the packet
        write!(output, "let mut buf = self.release();\n").unwrap();
        if self.packet().length().at(1).appear() || self.packet().length().at(2).appear() {
            // if we have variable packet length, we are gona need to trim off the trailing
            // bytes beyond the end of the packet
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
        if self.packet().length().at(1).appear() {
            write!(output, "let payload_len = buf.remaining();\n").unwrap();
        }

        let header_len_var = match self.packet().length().at(0) {
            LengthField::None => {
                // Header field is not defined, use the fixed header length.
                &header_len_name
            }
            LengthField::Undefined => {
                // Header field is defined without computing expression.
                // add a guard condition to make sure that the header_len is larger
                // than the fixed header length
                write!(output, "assert!(header.header_len()>={header_len_name});\n",).unwrap();
                "header.header_len()"
            }
            LengthField::Expr { expr } => {
                // Header field is defined with computing expression.
                let (field, _) = self.packet().header().field(expr.field_name()).unwrap();
                if field.default_fix {
                    // The default is fixed, check against the fixed value.
                    let default_val = match field.default {
                        DefaultVal::Num(n) => n,
                        _ => panic!(),
                    };
                    let fixed_header_len = expr.exec(default_val).unwrap();
                    write!(
                        output,
                        "assert!(header.header_len()=={fixed_header_len});\n",
                    )
                    .unwrap();
                } else {
                    // The default is not fixed, check with the fixed header length.
                    write!(output, "assert!(header.header_len()>={header_len_name});\n",).unwrap();
                }
                "header.header_len()"
            }
        };

        // move the cursor back and copy the packet content in
        write!(
            output,
            "buf.move_back({header_len_var});
(&mut buf.chunk_mut()[0..{header_len_name}]).copy_from_slice(header.as_bytes());
"
        )
        .unwrap();

        if !self.packet().length().at(1).appear() && !self.packet().length().at(2).appear() {
            // if we do not have variable payload length or packet length,
            // we can construct a packet variable and return it
            write!(output, "{packet_struct_name}::parse_unchecked(buf)\n",).unwrap();
        } else {
            let adjust_packet_length = if self.packet().length().at(1).appear() {
                "pkt.set_payload_len(payload_len);"
            } else if self.packet().length().at(2).appear() {
                "pkt.set_packet_len(pkt.buf.remaining());"
            } else {
                panic!()
            };

            // create a mutable packet variable
            write!(
                output,
                "let mut pkt = {packet_struct_name}::parse_unchecked(buf);
{adjust_packet_length}
pkt",
            )
            .unwrap();
        }

        write!(output, "}}\n").unwrap();
    }

    // Obtain a reference to the packet contained in the `header_impl`.
    fn packet(&self) -> &Packet {
        &self.header_impl.packet
    }

    // Obtain the type name of the packet struct.
    fn packet_struct_name(&self) -> String {
        self.packet().protocol_name().to_owned() + "Packet"
    }

    // Generate the `header` method.
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
}

/// Protocol header generator.
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
        self.header_gen(output);

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

    fn header_name(&self) -> String {
        self.packet.protocol_name().to_uppercase() + "_HEADER_TEMPLATE"
    }

    fn header_gen(&self, output: &mut dyn Write) {
        writeln!(
            output,
            "/// A fixed {} header.",
            self.packet.protocol_name()
        )
        .unwrap();
        write!(
            output,
            "pub const {}: {}<[u8;{}]> = [",
            self.header_name(),
            self.header_struct_name(),
            self.packet.header().header_len_in_bytes()
        )
        .unwrap();

        for (idx, b) in self.packet.header_template().iter().enumerate() {
            if idx < self.packet.header_template().len() - 1 {
                write!(output, "0x{:02x},", b).unwrap();
            } else {
                write!(output, "0x{:02x}];\n", b).unwrap()
            }
        }
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

// A helper struct for generating common struct definitino.
struct StructDefinition<'a> {
    struct_name: &'a str,
    derives: &'a [&'static str],
}

impl<'a> StructDefinition<'a> {
    // Generate the struct definition with derive attributes.
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
