use pktfmt::*;

#[macro_use]
mod common;

#[test]
fn print_header() {
    let file = "wtf.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::LengthBaseParser);

    println!("{:?}", res.unwrap());
}
