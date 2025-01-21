use std::io::Write;

use crate::ast::{DefaultVal, LengthField, Packet};

use super::header::HeaderImpl;
use super::{impl_block, GenerateFieldAccessMethod, StructDefinition};

/// Packet type generator.
pub struct PacketImpl<'a> {
    header_impl: HeaderImpl<'a, Packet>,
}

impl<'a> PacketImpl<'a> {
    pub fn new(packet: &'a Packet) -> Self {
        let header_impl = HeaderImpl::new(packet);
        Self { header_impl }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        self.header_impl.code_gen(output);

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

            // Generate the `option_bytes` method if the header length is variable.
            self.option_bytes(impl_block.get_writer());
        }

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

            // Generate the `option_bytes_mut` method if the header length is variable.
            self.option_bytes_mut(impl_block.get_writer())
        }
    }

    // A generator for the parse method.
    fn parse(&self, output: &mut dyn Write) {
        let packet_struct_name = self.packet_struct_name();
        let header_len_name = self.header_impl.header_len_name();

        // Dump the start of the function body.
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

        // Format checks are appended to the `guards`.
        let mut guards = Vec::new();
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
            // Add check for the variable payload length.
            guards.push(format!(
                "(packet.payload_len() as usize)+{header_len_var}>packet.buf.remaining()"
            ));
        } else if self.packet().length().at(2).appear() {
            // Add check for the variable packet length.
            guards.push(format!("(packet.packet_len() as usize)<{header_len_var}"));
            guards.push(format!(
                "(packet.packet_len() as usize)>packet.buf.remaining()"
            ));
        } else {
            // Do nothing
        }

        // Generate the checks.
        if guards.len() > 0 {
            let guard_str = Self::guard_assert_str(&guards, "||");
            write!(
                output,
                "if {guard_str} {{
return Err(packet.release());
}}
"
            )
            .unwrap();
        }

        write!(output, "Ok(packet)\n}}\n").unwrap();
    }

    // A generator for the `payload` method.
    fn payload(&self, output: &mut dyn Write) {
        // Dump the start of the function body.
        let header_len_name = self.header_impl.header_len_name();
        write!(
            output,
            "#[inline]
pub fn payload(self)->T{{
"
        )
        .unwrap();

        // If we have a variable payload or packet length, it is possible that the total
        // packet length will be smaller than the buffer size. In that case, we will
        // trim off the trailing bytes from the underlying buffer before we release the
        // payload.
        if self.packet().length().at(1).appear() {
            // The protocol has variable payload length.
            let header_len_var = if self.packet().length().at(0).appear() {
                "(self.header_len() as usize)"
            } else {
                &header_len_name
            };
            write!(
                output,
                "assert!({header_len_var}+self.payload_len() as usize<=self.buf.remaining());
let trim_size = self.buf.remaining()-({header_len_var}+self.payload_len() as usize);
"
            )
            .unwrap();
        } else if self.packet().length().at(2).appear() {
            // The protocol has variable packet length.
            write!(
                output,
                "assert!((self.packet_len() as usize)<=self.buf.remaining());
let trim_size = self.buf.remaining()-self.packet_len() as usize;
"
            )
            .unwrap();
        } else {
            // Do nothing.
        }

        let header_len_var = if self.packet().length().at(0).appear() {
            // Here, we have variable header length, so we must save the length
            // to a local variable before we release the buffer.
            write!(output, "let header_len = self.header_len() as usize;\n").unwrap();
            "header_len"
        } else {
            // The header has fixed length, we use the pre-defined constant.
            &header_len_name
        };

        // Release the internal buffer from the packet and then trim off the trailing
        // bytes if necessary.
        write!(output, "let mut buf = self.release();\n").unwrap();
        if self.packet().length().at(1).appear() || self.packet().length().at(2).appear() {
            write!(
                output,
                "if trim_size > 0 {{
buf.trim_off(trim_size);
}}
"
            )
            .unwrap();
        }

        // Advance the cursor beyond the header.
        write!(
            output,
            "buf.advance({header_len_var});
buf
}}
"
        )
        .unwrap();
    }

    // A generator for the `prepend_header` method.
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

        let mut guards = Vec::new();
        let header_len_var = match self.packet().length().at(0) {
            LengthField::None => {
                // Fixed header length.
                guards.push(format!("buf.chunk_headeroom()>={header_len_name}"));
                &header_len_name
            }
            LengthField::Undefined => {
                // Variable header length with undefined computing expression.
                write!(output, "let header_len = header.header_len() as usize;\n").unwrap();
                guards.push(format!("header_len>={header_len_name}"));
                guards.push(format!("header_len<=buf.chunk_headeroom()"));
                "header_len"
            }
            LengthField::Expr { expr } => {
                // Header field is defined with computing expression.
                write!(output, "let header_len = header.header_len() as usize;\n").unwrap();
                let (field, _) = self.packet().header().field(expr.field_name()).unwrap();
                if field.default_fix {
                    // Variable header length with fixed default value.
                    let default_val = match field.default {
                        DefaultVal::Num(n) => n,
                        _ => panic!(),
                    };
                    let fixed_header_len = expr.exec(default_val).unwrap();
                    guards.push(format!("header_len=={fixed_header_len}"));
                } else {
                    // Variable header length.
                    guards.push(format!("header_len>={header_len_name}"));
                }
                guards.push(format!("header_len<=buf.chunk_headroom()"));
                "header_len"
            }
        };
        write!(
            output,
            "assert!({});\n",
            Self::guard_assert_str(&guards, "&&")
        )
        .unwrap();

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

    // A generator for the option bytes.
    fn option_bytes(&self, output: &mut dyn Write) {
        match self.packet().length().at(0) {
            LengthField::None => {} //do nothing
            _ => {
                write!(
                    output,
                    "#[inline]
        pub fn option_bytes(&self)->&[u8]{{
        &self.buf.chunk()[{}..(self.header_len() as usize)]
        }}
        ",
                    self.header_impl.header_len_name()
                )
                .unwrap();
            }
        }
    }

    // A generator for the mutable option bytes.
    fn option_bytes_mut(&self, output: &mut dyn Write) {
        match self.packet().length().at(0) {
            LengthField::None => {} //do nothing
            _ => {
                write!(
                    output,
                    "#[inline]
        pub fn option_bytes_mut(&mut self)->&mut [u8]{{
        &mut self.buf.chunk_mut()[{}..(self.header_len() as usize)]
        }}
        ",
                    self.header_impl.header_len_name()
                )
                .unwrap();
            }
        }
    }

    // Obtain a reference to the packet contained in the `header_impl`.
    fn packet(&self) -> &Packet {
        self.header_impl.get_inner()
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

    fn guard_assert_str(guards: &Vec<String>, comp: &str) -> String {
        if guards.len() == 1 {
            format!("{}", guards[0])
        } else {
            let mut buf = Vec::new();
            guards.iter().enumerate().for_each(|(idx, s)| {
                write!(&mut buf, "({s})").unwrap();

                if idx < guards.len() - 1 {
                    write!(&mut buf, "{comp}").unwrap();
                }
            });
            String::from_utf8(buf).unwrap()
        }
    }
}
