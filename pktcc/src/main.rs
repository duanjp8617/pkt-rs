mod ast_;
mod ast1;

mod ast;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub pktfmt); 

fn main() {
    println!("Hello, world!");
}

#[test]
fn simpleval() {
    let res = pktfmt::PrimitiveParser::new().parse("25566").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("u8").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("u16").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("u32").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("u64").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("&[ u8   ]").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("true").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("false").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("false_sdf_sdfsadf_").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("False_000sdf_sdfsadf_").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("4 * (ident - 2)").unwrap();
    println!("{:?}", res);

}

#[test]
fn simpleval_cmp() {
    let res = pktfmt::PrimitiveParser::new().parse("type == 2").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("(type == 2)").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("!(type == 2)").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("!(type == 2) && !(type == 3)").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("(!(type == 2)) && !(type == 3)").unwrap();
    println!("{:?}", res);

    let res = pktfmt::PrimitiveParser::new().parse("(type != 2) && (type != 3)").unwrap();
    println!("{:?}", res);
}

#[test]
fn simpleval_rsexpr() {
    let res = pktfmt::PrimitiveParser::new().parse(r#"rs"IpProtocol::UDP""#).unwrap();
    println!("{:?}", res);
}