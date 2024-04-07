use pktfmt::*;

#[macro_use]
mod common;

#[test]
fn print_header() {
   let file = "header.pktfmt";

    let res = parse_for_result!(file, pktfmt::parser::HeaderParser);

    println!("{:?}", res.unwrap());
}