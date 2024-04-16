use pktfmt::{codegen::FieldAccessMethodBlob, *};

#[macro_use]
mod common;

#[test]
fn print_header() {
    let file = "ipv6.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::PacketParser);
    let packet = res.unwrap();
    let mut buf: Vec<u8> = Vec::new();

    // packet.field_list.iter().for_each(|(name, field)| {
    //     let (bitpos, _) = packet.field_pos_map.get(name).unwrap();
    //     codegen::header_field_get_method(name, field, *bitpos, &mut buf);
    // });

    // println!("{}", std::str::from_utf8(&buf[..]).unwrap());

    let type_name = packet.protocol_name.clone() + "Header";
    let trait_name = "AsMut<[u8]>";
    let target_slice = "self.buf.as_mut()";
    let write_value = Some("value".into());

    FieldAccessMethodBlob::new(
        &packet.field_list,
        &packet.field_pos_map,
        type_name,
        trait_name.into(),
        target_slice.into(),
        write_value,
    )
    .code_gen(&mut buf);

    println!("{}", std::str::from_utf8(&buf[..]).unwrap());
}

#[test]
fn fuck() {
    let mut f = [0; 3];
    f[0] = 5;

    println!("{:?}", &f[..]);
}
