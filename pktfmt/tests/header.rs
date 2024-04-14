use pktfmt::*;

#[macro_use]
mod common;

#[test]
fn print_header() {
    let file = "wtf.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::PacketParser);
    let packet = res.unwrap();
    let mut buf: Vec<u8> = Vec::new();

    packet.field_list.iter().for_each(|(name, field)| {
        let (bitpos, _) = packet.field_pos_map.get(name).unwrap();
        codegen::packet_field_get_method(name, field, *bitpos, &mut buf);
    });

    println!("{}", std::str::from_utf8(&buf[..]).unwrap());
}

#[test]
fn fuck() {
    let mut f = [0; 3];
    f[0] = 5;

    println!("{:?}", &f[..]);
}
