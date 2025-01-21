use std::{default, io::Write};

use crate::ast::{DefaultVal, Length, LengthField, Message};

use super::header::HeaderImpl;
use super::{impl_block, GenerateFieldAccessMethod, StructDefinition};

/// Packet type generator.
pub struct MessageImpl<'a> {
    header_impl: HeaderImpl<'a, Message>,
}

impl<'a> MessageImpl<'a> {
    pub fn new(msg: &'a Message) -> Self {
        let header_impl = HeaderImpl::new(msg);
        Self { header_impl }
    }

    pub fn code_gen(&self, mut output: &mut dyn Write) {
        self.header_impl.code_gen(output);
        

        // Generate packet struct definition.
        let msg_struct_gen = StructDefinition {
            struct_name: &self.message_struct_name(),
            derives: &["Debug"],
        };
        msg_struct_gen.code_gen(output);

        {
            let mut impl_block = impl_block(
                "T:AsRef<[u8]>",
                &self.message_struct_name(),
                "T",
                &mut output,
            );
        }
    }

    // Obtain a reference to the message contained in the `header_impl`.
    fn msg(&self) -> &Message {
        self.header_impl.get_inner()
    }

    // Obtain the type name of the message struct.
    fn message_struct_name(&self) -> String {
        self.msg().protocol_name().to_owned() + "Message"
    }
}
