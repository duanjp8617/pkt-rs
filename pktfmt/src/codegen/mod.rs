use std::io::Write;

use crate::ast::{Message, Packet, LENGTH_TEMPLATE_FOR_HEADER};

mod container;
use container::*;

mod field;
use field::*;

mod length;
use length::*;

mod parse;
use parse::*;

mod payload;
use payload::*;

mod build;
use build::*;

// A writer object that appends prefix string and prepends suffix string to the
// underlying content.
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

pub struct HeaderGen<'a> {
    packet: &'a Packet,
}

impl<'a> HeaderGen<'a> {
    pub fn new(packet: &'a Packet) -> Self {
        Self { packet }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        // Header length const.
        self.code_gen_for_header_len_const(output);

        // Header template.
        self.code_gen_for_header_template(output);

        // Defines the header struct.
        let header_struct_gen = Container {
            container_struct_name: &self.header_struct_name(),
            derives: &["Debug", "Clone", "Copy"],
        };
        header_struct_gen.code_gen(output);
        let fields = FieldGenerator::new(self.packet.header());
        let length = LengthGenerator::new(self.packet.header(), self.packet.length());
        let parse = Parse::new(self.packet.header(), LENGTH_TEMPLATE_FOR_HEADER);

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]>",
                &self.header_struct_name(),
                "T",
                &mut output,
            );

            // Basic container buffer conversion.
            Container::code_gen_for_parse_unchecked("buf", "T", impl_block.get_writer());
            Container::code_gen_for_buf("buf", "T", impl_block.get_writer());
            Container::code_gen_for_release("buf", "T", impl_block.get_writer());

            // Header parse with format checking.
            parse.code_gen_for_contiguous_buffer(
                "parse",
                "buf",
                "T",
                ".as_ref()",
                impl_block.get_writer(),
            );

            // Header slice.
            Container::code_gen_for_header_slice(
                "header_slice",
                "&",
                ".buf.as_ref()",
                &format!("{}", self.packet.header().header_len_in_bytes()),
                impl_block.get_writer(),
            );

            // Field getters.
            fields.code_gen("self.buf.as_ref()", None, impl_block.get_writer());

            // Length field getters.
            length.code_gen("self.buf.as_ref()", None, impl_block.get_writer())
        }

        {
            let mut impl_block = impl_block(
                "T:AsMut<[u8]>",
                &self.header_struct_name(),
                "T",
                &mut output,
            );

            // Mutable header slice.
            Container::code_gen_for_header_slice(
                "header_slice_mut",
                "&mut ",
                ".buf.as_mut()",
                &format!("{}", self.packet.header().header_len_in_bytes()),
                impl_block.get_writer(),
            );

            // Field setters.
            fields.code_gen("self.buf.as_mut()", Some("value"), impl_block.get_writer());

            // Length field setters.
            length.code_gen("self.buf.as_mut()", Some("value"), impl_block.get_writer());
        }
    }

    // Return the name of the header length const.
    fn header_len_const_name(&self) -> String {
        self.packet.protocol_name().to_uppercase() + "_HEADER_LEN"
    }

    // Return the name of the header struct.
    fn header_struct_name(&self) -> String {
        self.packet.protocol_name().to_string() + "Header"
    }

    // Return the name of the fixed header array.
    fn header_template_name(&self) -> String {
        self.packet.protocol_name().to_uppercase() + "_HEADER_TEMPLATE"
    }

    fn code_gen_for_header_len_const(&self, output: &mut dyn Write) {
        let header_len = self.packet.header().header_len_in_bytes();

        write!(
            output,
            "/// A constant that defines the fixed byte length of the {} protocol header.
pub const {}: usize = {header_len};
",
            &self.packet.protocol_name(),
            self.header_len_const_name(),
        )
        .unwrap();
    }

    fn code_gen_for_header_template(&self, output: &mut dyn Write) {
        writeln!(
            output,
            "/// A fixed {} header.",
            self.packet.protocol_name()
        )
        .unwrap();
        write!(
            output,
            "pub const {}: {}<[u8;{}]> = {} {{ buf: [",
            self.header_template_name(),
            self.header_struct_name(),
            self.packet.header().header_len_in_bytes(),
            self.header_struct_name(),
        )
        .unwrap();

        for (idx, b) in self.packet.header().header_template().iter().enumerate() {
            if idx < self.packet.header().header_template().len() - 1 {
                write!(output, "0x{:02x},", b).unwrap();
            } else {
                write!(output, "0x{:02x}] }};\n", b).unwrap()
            }
        }
    }
}

/// Packet type generator.
pub struct PacketGen<'a> {
    header_gen: HeaderGen<'a>,
}

impl<'a> PacketGen<'a> {
    pub fn new(packet: &'a Packet) -> Self {
        Self {
            header_gen: HeaderGen::new(packet),
        }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        // Defines the header struct.
        let packet_struct_gen = Container {
            container_struct_name: &self.packet_struct_name(),
            derives: &["Debug", "Clone", "Copy"],
        };
        packet_struct_gen.code_gen(output);

        let fields = FieldGenerator::new(self.packet().header());
        let length = LengthGenerator::new(self.packet().header(), self.packet().length());
        let parse = Parse::new(self.packet().header(), self.packet().length().as_slice());
        let payload = Payload::new(self.packet().header(), self.packet().length().as_slice());
        let build = Build::new(self.packet().header(), self.packet().length().as_slice());

        {
            let mut impl_block =
                impl_block("T:PktBuf", &self.packet_struct_name(), "T", &mut output);

            // Basic container-buffer conversion.
            Container::code_gen_for_parse_unchecked("buf", "T", impl_block.get_writer());
            Container::code_gen_for_buf("buf", "T", impl_block.get_writer());
            Container::code_gen_for_release("buf", "T", impl_block.get_writer());

            // Packet parse with format checking.
            parse.code_gen_for_pktbuf("parse", "buf", "T", impl_block.get_writer());

            // Packet payload.
            payload.code_gen_for_pktbuf("payload", "buf", "T", impl_block.get_writer());

            // Fixed length header slice.
            Container::code_gen_for_header_slice(
                "header_slice",
                "&",
                ".buf.chunk()",
                &format!("{}", self.packet().header().header_len_in_bytes()),
                impl_block.get_writer(),
            );

            // Option slice.
            if self.packet().length().at(0).appear() {
                Container::code_gen_for_option_slice(
                    "option_slice",
                    "&",
                    ".buf.chunk()",
                    &format!("{}", self.packet().header().header_len_in_bytes()),
                    impl_block.get_writer(),
                );
            }

            // Field getters.
            fields.code_gen("self.buf.chunk()", None, impl_block.get_writer());

            // Length field setters.
            length.code_gen("self.buf.chunk()", None, impl_block.get_writer());
        }

        {
            let mut impl_block =
                impl_block("T:BufMut", &self.packet_struct_name(), "T", &mut output);

            // Packet build.
            build.code_gen_for_pktbuf(
                "prepend_header",
                "HT:AsRef<[u8]>",
                "buf",
                "T",
                "header",
                &format!("&{}<HT>", self.header_gen.header_struct_name()),
                impl_block.get_writer(),
            );

            // Mutable option slice.
            if self.packet().length().at(0).appear() {
                Container::code_gen_for_option_slice(
                    "option_slice_mut",
                    "&mut ",
                    ".buf.chunk_mut()",
                    &format!("{}", self.packet().header().header_len_in_bytes()),
                    impl_block.get_writer(),
                );
            }

            // Field setters.
            fields.code_gen(
                "self.buf.chunk_mut()",
                Some("value"),
                impl_block.get_writer(),
            );

            // Length field setters.
            length.code_gen(
                "self.buf.chunk_mut()",
                Some("value"),
                impl_block.get_writer(),
            );
        }

        {
            let mut impl_block =
                impl_block("'a", &self.packet_struct_name(), "Cursor<'a>", &mut output);

            // Specialized parse for Cursor with format checking.
            parse.code_gen_for_contiguous_buffer(
                "parse_for_cursor",
                "buf",
                "Cursor<'a>",
                ".chunk()",
                impl_block.get_writer(),
            );

            // Specialized payload for Cursor.
            let f = |writer: &mut dyn Write, s: &str| {
                let mut ht_writer = HeadTailWriter::new(writer, "Cursor::new(", ")\n");
                write!(ht_writer.get_writer(), "{s}").unwrap();
            };
            payload.code_gen_for_contiguous_buffer(
                "payload_for_cursor",
                "&",
                "buf",
                "Cursor<'_>",
                "chunk()",
                f,
                impl_block.get_writer(),
            );
        }

        {
            let mut impl_block = impl_block(
                "'a",
                &self.packet_struct_name(),
                "CursorMut<'a>",
                &mut output,
            );

            // Specialized parse for CursorMut with format checking.
            parse.code_gen_for_contiguous_buffer(
                "parse_for_cursor_mut",
                "buf",
                "CursorMut<'a>",
                ".chunk()",
                impl_block.get_writer(),
            );

            // Specialized payload for CursorMut.
            let f = |writer: &mut dyn Write, s: &str| {
                let mut ht_writer = HeadTailWriter::new(writer, "CursorMut::new(", ")\n");
                write!(ht_writer.get_writer(), "{s}").unwrap();
            };
            payload.code_gen_for_contiguous_buffer(
                "payload_for_cursor_mut",
                "&mut ",
                "buf",
                "CursorMut<'_>",
                "chunk_mut()",
                f,
                impl_block.get_writer(),
            );
        }
    }

    // Obtain a reference to the packet contained in the `header_impl`.
    fn packet(&self) -> &Packet {
        self.header_gen.packet
    }

    // Obtain the type name of the packet struct.
    fn packet_struct_name(&self) -> String {
        self.packet().protocol_name().to_owned() + "Packet"
    }
}

pub struct MessageGen<'a> {
    message: &'a Message,
}

impl<'a> MessageGen<'a> {
    pub fn new(message: &'a Message) -> Self {
        Self { message }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        self.code_gen_for_header_array(output);

        // Defines the header struct.
        let header_struct_gen = Container {
            container_struct_name: &self.message_struct_name(),
            derives: &["Debug", "Clone", "Copy"],
        };
        header_struct_gen.code_gen(output);

        let fields = FieldGenerator::new(self.message.header());
        let length = LengthGenerator::new(self.message.header(), self.message.length());
        let parse = Parse::new(self.message.header(), self.message.length().as_slice());
        let payload = Payload::new(self.message.header(), self.message.length().as_slice());
        let build = Build::new(self.message.header(), self.message.length().as_slice());

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]>",
                &self.message_struct_name(),
                "T",
                &mut output,
            );

            // Bsasic container buffer conversion.
            Container::code_gen_for_parse_unchecked("buf", "T", impl_block.get_writer());
            Container::code_gen_for_buf("buf", "T", impl_block.get_writer());
            Container::code_gen_for_release("buf", "T", impl_block.get_writer());

            // Parse with format checking.
            parse.code_gen_for_contiguous_buffer(
                "parse",
                "buf",
                "T",
                ".as_ref()",
                impl_block.get_writer(),
            );

            // Message payload.
            payload.code_gen_for_contiguous_buffer(
                "payload",
                "&",
                "buf",
                "&[u8]",
                "as_ref()",
                |writer, s| write!(writer, "{s}").unwrap(),
                impl_block.get_writer(),
            );

            // Option slice.
            if self.message.length().at(0).appear() {
                Container::code_gen_for_option_slice(
                    "option_slice",
                    "&",
                    ".buf.as_ref()",
                    &format!("{}", self.message.header().header_len_in_bytes()),
                    impl_block.get_writer(),
                );
            }

            // Field getters.
            fields.code_gen("self.buf.as_ref()", None, impl_block.get_writer());

            // Length field setters.
            length.code_gen("self.buf.as_ref()", None, impl_block.get_writer())
        }

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]> + AsMut<[u8]>",
                &self.message_struct_name(),
                "T",
                &mut output,
            );

            // Message mutable payload.
            payload.code_gen_for_contiguous_buffer(
                "payload_mut",
                "&mut ",
                "buf",
                "&mut [u8]",
                "as_mut()",
                |writer, s| write!(writer, "{s}").unwrap(),
                impl_block.get_writer(),
            );

            // Message build.
            build.code_gen_for_contiguous_buffer(
                "build_message",
                "buf",
                "T",
                "as_mut()",
                &self.header_array_name(),
                impl_block.get_writer(),
            );

            // Mutable option slice.
            if self.message.length().at(0).appear() {
                Container::code_gen_for_option_slice(
                    "option_slice_mut",
                    "&mut ",
                    ".buf.as_mut()",
                    &format!("{}", self.message.header().header_len_in_bytes()),
                    impl_block.get_writer(),
                );
            }

            // Field setters.
            fields.code_gen("self.buf.as_mut()", Some("value"), impl_block.get_writer());

            // Length field setters.
            length.code_gen("self.buf.as_mut()", Some("value"), impl_block.get_writer());
        }
    }

    // Return the name of the fixed header array.
    fn header_array_name(&self) -> String {
        self.message.protocol_name().to_uppercase() + "_HEADER_ARRAY"
    }

    // Return the name of the header struct.
    fn message_struct_name(&self) -> String {
        self.message.protocol_name().to_string() + "Message"
    }

    fn code_gen_for_header_array(&self, output: &mut dyn Write) {
        writeln!(
            output,
            "/// A fixed {} header array.",
            self.message.protocol_name()
        )
        .unwrap();
        write!(
            output,
            "pub const {}: [u8;{}] = [",
            self.header_array_name(),
            self.message.header().header_len_in_bytes(),
        )
        .unwrap();

        for (idx, b) in self.message.header().header_template().iter().enumerate() {
            if idx < self.message.header().header_template().len() - 1 {
                write!(output, "0x{:02x},", b).unwrap();
            } else {
                write!(output, "0x{:02x}];\n", b).unwrap()
            }
        }
    }
}

pub struct PacketGenForContiguousBuf<'a> {
    packet: &'a Packet,
}

impl<'a> PacketGenForContiguousBuf<'a> {
    pub fn new(packet: &'a Packet) -> Self {
        Self { packet }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        self.code_gen_for_header_array(output);

        // Defines the header struct.
        let header_struct_gen = Container {
            container_struct_name: &self.packet_struct_name(),
            derives: &["Debug", "Clone", "Copy"],
        };
        header_struct_gen.code_gen(output);

        let fields = FieldGenerator::new(self.packet.header());
        let length = LengthGenerator::new(self.packet.header(), self.packet.length());
        let parse = Parse::new(self.packet.header(), self.packet.length().as_slice());
        let payload = Payload::new(self.packet.header(), self.packet.length().as_slice());
        let build = Build::new(self.packet.header(), self.packet.length().as_slice());

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]>",
                &self.packet_struct_name(),
                "T",
                &mut output,
            );

            // Bsasic container buffer conversion.
            Container::code_gen_for_parse_unchecked("buf", "T", impl_block.get_writer());
            Container::code_gen_for_buf("buf", "T", impl_block.get_writer());
            Container::code_gen_for_release("buf", "T", impl_block.get_writer());

            // Parse with format checking.
            parse.code_gen_for_contiguous_buffer(
                "parse",
                "buf",
                "T",
                ".as_ref()",
                impl_block.get_writer(),
            );

            // Message payload.
            payload.code_gen_for_contiguous_buffer(
                "payload",
                "&",
                "buf",
                "&[u8]",
                "as_ref()",
                |writer, s| write!(writer, "{s}").unwrap(),
                impl_block.get_writer(),
            );

            // Header slice.
            Container::code_gen_for_header_slice(
                "header_slice",
                "&",
                ".buf.as_ref()",
                &format!("{}", self.packet.header().header_len_in_bytes()),
                impl_block.get_writer(),
            );

            // Option slice.
            if self.packet.length().at(0).appear() {
                Container::code_gen_for_option_slice(
                    "option_slice",
                    "&",
                    ".buf.as_ref()",
                    &format!("{}", self.packet.header().header_len_in_bytes()),
                    impl_block.get_writer(),
                );
            }

            // Field getters.
            fields.code_gen("self.buf.as_ref()", None, impl_block.get_writer());

            // Length field setters.
            length.code_gen("self.buf.as_ref()", None, impl_block.get_writer())
        }

        {
            let mut impl_block = impl_block(
                "'a, T:AsRef<[u8]> + ?Sized",
                &self.packet_struct_name(),
                "&'a T",
                &mut output,
            );

            // Message payload.
            payload.code_gen_for_contiguous_buffer(
                "payload_with_extended_lifetime",
                "&",
                "buf",
                "&'a [u8]",
                "as_ref()",
                |writer, s| write!(writer, "{s}").unwrap(),
                impl_block.get_writer(),
            );
        }

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]> + AsMut<[u8]>",
                &self.packet_struct_name(),
                "T",
                &mut output,
            );

            // Message mutable payload.
            payload.code_gen_for_contiguous_buffer(
                "payload_mut",
                "&mut ",
                "buf",
                "&mut [u8]",
                "as_mut()",
                |writer, s| write!(writer, "{s}").unwrap(),
                impl_block.get_writer(),
            );

            // Message build.
            build.code_gen_for_contiguous_buffer(
                "build_packet",
                "buf",
                "T",
                "as_mut()",
                &self.header_array_name(),
                impl_block.get_writer(),
            );

            // Mutable option slice.
            if self.packet.length().at(0).appear() {
                Container::code_gen_for_option_slice(
                    "option_slice_mut",
                    "&mut ",
                    ".buf.as_mut()",
                    &format!("{}", self.packet.header().header_len_in_bytes()),
                    impl_block.get_writer(),
                );
            }

            // Field setters.
            fields.code_gen("self.buf.as_mut()", Some("value"), impl_block.get_writer());

            // Length field setters.
            length.code_gen("self.buf.as_mut()", Some("value"), impl_block.get_writer());
        }
    }

    // Return the name of the fixed header array.
    fn header_array_name(&self) -> String {
        self.packet.protocol_name().to_uppercase() + "_HEADER_ARRAY"
    }

    // Return the name of the header struct.
    fn packet_struct_name(&self) -> String {
        self.packet.protocol_name().to_string() + "Packet"
    }

    fn code_gen_for_header_array(&self, output: &mut dyn Write) {
        writeln!(
            output,
            "/// A fixed {} header array.",
            self.packet.protocol_name()
        )
        .unwrap();
        write!(
            output,
            "pub const {}: [u8;{}] = [",
            self.header_array_name(),
            self.packet.header().header_len_in_bytes(),
        )
        .unwrap();

        for (idx, b) in self.packet.header().header_template().iter().enumerate() {
            if idx < self.packet.header().header_template().len() - 1 {
                write!(output, "0x{:02x},", b).unwrap();
            } else {
                write!(output, "0x{:02x}];\n", b).unwrap()
            }
        }
    }
}
