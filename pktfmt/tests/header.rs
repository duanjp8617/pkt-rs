use pktfmt::{codegen::FieldAccessMethod, *};

#[macro_use]
mod common;

#[test]
fn print_header() {
    let file = "ipv6.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::PacketParser);
    let packet = res.unwrap();
    let mut buf: Vec<u8> = Vec::new();

    let type_name = packet.protocol_name.clone() + "Packet";
    let trait_name = "PktMut";
    let target_slice = "self.buf.chunk_mut()";
    let write_value = "value";

    packet.set_method_gen(&type_name, trait_name, target_slice, write_value, &mut buf);
    // packet.get_method_gen(&type_name, trait_name, target_slice, &mut buf);

    println!("{}", std::str::from_utf8(&buf[..]).unwrap());
}

#[test]
fn fuck() {
    let mut f = [0; 3];
    f[0] = 5;

    println!("{:?}", &f[..]);
}
