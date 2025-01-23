use std::io::Write;

use super::HeadTailWriter;

/// A generator for a generic container type.
///
/// It generates the following type, which just wraps a generic variable named
/// `buf`:
///  pub struct name<T> {
///     buf: T
/// }
pub struct Container<'a> {
    pub name: &'a str,
    pub derives: &'a [&'static str],
}

impl<'a> Container<'a> {
    // Generate the struct definition with derive attributes.
    pub fn code_gen(&self, mut output: &mut dyn Write) {
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
            self.name
        )
        .unwrap();
    }

    // Wrap a `buf` inside a container.
    pub fn parse_unchecked(output: &mut dyn Write) {
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
    pub fn buf(output: &mut dyn Write) {
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

    // Release the `buf` from the container.
    pub fn release(output: &mut dyn Write) {
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
