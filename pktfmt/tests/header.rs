use pktfmt::{codegen::*, *};

#[macro_use]
mod common;

#[test]
fn print_header() {
    let file = "ipv6.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::PacketParser);
    let packet = res.unwrap();
    let mut buf: Vec<u8> = Vec::new();

    let hl = HeaderImpl::new(&packet);
    PacketImpl::new(&hl).code_gen(&mut buf);
    // hl.code_gen(&mut buf);
    // packet.get_method_gen(&type_name, trait_name, target_slice, &mut buf);
    // packet.header_base_gen(&mut buf);

    println!("{}", std::str::from_utf8(&buf[..]).unwrap());
    let bytes = [0, 1];
    u16::from_be_bytes(&bytes[..]);
    u32::from_be_bytes(&bytes[..]);
}

#[test]
fn fuck() {
    let f = vec![1, 2, 3];
    let f_ref = &f;
    drop(f);
    println!("{}", f_ref[0]);
}
