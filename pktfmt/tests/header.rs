use pktfmt::{codegen::*, *};

#[macro_use]
mod common;

#[test]
fn print_header() {
    let file = "wtf.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::PacketParser);
    let packet = res.unwrap();
    let mut buf: Vec<u8> = Vec::new();

    let hl = HeaderImpl::new(&packet);
    PacketImpl::new(&hl).code_gen(&mut buf);
    // packet.get_method_gen(&type_name, trait_name, target_slice, &mut buf);
    // packet.header_base_gen(&mut buf);

    println!("{}", std::str::from_utf8(&buf[..]).unwrap());
}

#[test]
fn fuck() {
    let mut f = [0; 3];
    f[0] = 5;

    println!("{:?}", &f[..]);
}
