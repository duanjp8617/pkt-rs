#[macro_use]
extern crate quick_error;

mod ast;
mod error;
mod file_text;
mod token;
mod utils;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);

use file_text::FileText;
use std::io;

fn main() {
    let f = FileText::new("/home/djp/test.txt").unwrap();

    let mut stdout = io::stdout();

    f.render_code_block(3, 10, &mut stdout).unwrap();
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
