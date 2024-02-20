#[macro_use]
extern crate quick_error;

#[macro_use]
mod utils;

mod ast;
mod file_text;
mod token;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);

use file_text::FileText;
use std::io;

fn main() {
    let f = FileText::new("/home/djp/pkt-rs/pktcc/tests/field1.pktfmt").unwrap();

    let tokenizer = token::Tokenizer::new(f.text());

    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &f);

    let mut out = std::io::stderr();
    match parse_res {
        Ok(field) => println!("{:?}", field),
        Err(err) => utils::render_error(&f, err, &mut out),
    }
}

// #[test]
// fn simpleval() {
//     let res = parser::PrimitiveParser::new().parse("25566").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("u8").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("u16").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("u32").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("u64").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("&[ u8   ]").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("true").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("false").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("false_sdf_sdfsadf_")
//         .unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("False_000sdf_sdfsadf_")
//         .unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("4 * (ident - 2)")
//         .unwrap();
//     println!("{:?}", res);
// }

// #[test]
// fn simpleval_cmp() {
//     let res = parser::PrimitiveParser::new().parse("type == 2").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new().parse("(type == 2)").unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("!(type == 2)")
//         .unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("!(type == 2) && !(type == 3)")
//         .unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("(!(type == 2)) && !(type == 3)")
//         .unwrap();
//     println!("{:?}", res);

//     let res = parser::PrimitiveParser::new()
//         .parse("(type != 2) && (type != 3)")
//         .unwrap();
//     println!("{:?}", res);
// }

// #[test]
// fn simpleval_rsexpr() {
//     let res = parser::PrimitiveParser::new()
//         .parse(r#"rs"IpProtocol::UDP""#)
//         .unwrap();
//     println!("{:?}", res);
// }

// #[test]
// fn udp() {
//     let res = parser::DefinitionParser::new()
//         .parse(
//             r#"
//     packet Udp {
//         header = [
//             src_port = Field {bit = 16},
//             dst_port = Field {bit = 16},
//             length = Field {
//                 bit = 16,
//                 default = 8,
//                 gen = false,
//             },
//             checksum = Field {bit = 16},
//         ],
//         with_payload = true,
//         packet_len = PacketLen {
//             expr = length,
//             mult = 1,
//             min = 8,
//             max = 65535,
//         }
//     }
//     "#,
//         )
//         .unwrap();
//     println!("{:?}", res);
// }
