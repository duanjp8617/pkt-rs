use std::io::Write;

use super::{impl_block, GenerateFieldAccessMethod, StructDefinition};

/// Generate fixed-length protocol header.
pub struct HeaderImpl<'a, T> {
    inner: &'a T,
}

impl<'a, T: GenerateFieldAccessMethod> HeaderImpl<'a, T> {
    pub fn new(inner: &'a T) -> Self {
        Self { inner }
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
            self.inner
                .header_field_method_gen("self.buf.as_ref()", None, impl_block.get_writer());

            // Generate get methods for length fields if there are any.
            self.inner
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
            self.inner.header_field_method_gen(
                "self.buf.as_mut()",
                Some("value"),
                impl_block.get_writer(),
            );

            // Generate set methods for length fields if there are any.
            self.inner.length_field_method_gen(
                "self.buf.as_mut()",
                Some("value"),
                impl_block.get_writer(),
            );
        }
    }

    // Return the name of the header length const.
    pub(crate) fn header_len_name(&self) -> String {
        self.inner.protocol_name().to_uppercase() + "_HEADER_LEN"
    }

    // Return the name of the header struct.
    pub(crate) fn header_struct_name(&self) -> String {
        self.inner.protocol_name().to_string() + "Header"
    }

    pub(crate) fn get_inner(&self) -> &'a T {
        self.inner
    }

    // Return the name of the fixed header array.
    fn header_name(&self) -> String {
        self.inner.protocol_name().to_uppercase() + "_HEADER_TEMPLATE"
    }

    fn header_len_gen(&self, output: &mut dyn Write) {
        let header_len = self.inner.header().header_len_in_bytes();

        write!(
            output,
            "/// A constant that defines the fixed byte length of the {} protocol header.
pub const {}: usize = {header_len};
",
            &self.inner.protocol_name(),
            self.header_len_name(),
        )
        .unwrap();
    }

    fn header_gen(&self, output: &mut dyn Write) {
        writeln!(output, "/// A fixed {} header.", self.inner.protocol_name()).unwrap();
        write!(
            output,
            "pub const {}: {}<[u8;{}]> = [",
            self.header_name(),
            self.header_struct_name(),
            self.inner.header().header_len_in_bytes()
        )
        .unwrap();

        for (idx, b) in self.inner.header().header_template().iter().enumerate() {
            if idx < self.inner.header().header_template().len() - 1 {
                write!(output, "0x{:02x},", b).unwrap();
            } else {
                write!(output, "0x{:02x}];\n", b).unwrap()
            }
        }
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
