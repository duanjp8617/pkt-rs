use std::io::Write;

use crate::ast::Packet;

use super::{GenerateFieldAccessMethod, HeadTailWriter};

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
