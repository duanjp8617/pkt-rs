#[allow(unused_imports)]
use std::io::{Write, Read};

use pktfmt::*;

#[allow(dead_code)]
macro_rules! test_parse_error {
    (   
        $file_name: expr,
        $parser: ty
    ) => {
        {
            // The test is executed under the crate root directory.
            let mut program_path = std::env::current_dir().unwrap();
            program_path.push("tests");
            program_path.push("parse_errors_input");
            program_path.push($file_name);

            let file_text = ::pktfmt::file_text::FileText::new(program_path.as_path()).unwrap();
            let tokenizer = ::pktfmt::token::Tokenizer::new(file_text.text());
            let parse_res = ::pktfmt::parse_with_error!($parser, tokenizer);

            let mut out: Vec<u8> = ::std::vec::Vec::new();
            ::pktfmt::utils::render_error(&file_text, parse_res.unwrap_err(), &mut out);
            

            let mut error_path = std::env::current_dir().unwrap();
            error_path.push("tests");
            error_path.push("parse_errors_input");
            error_path.push(format!("{}.stderr", $file_name));

            let mut f = std::fs::File::open(error_path).unwrap();
            let mut err_text = String::new();
            f.read_to_string(&mut err_text).unwrap();

            assert_eq!(std::str::from_utf8(&out[..]).unwrap(), &err_text);
        }
    };
}

#[allow(unused_macros)]
macro_rules! dump_parse_error {
    (   
        $file_name: expr,
        $parser: ty
    ) => {
        {
            // The test is executed under the crate root directory.
            let mut program_path = std::env::current_dir().unwrap();
            program_path.push("tests");
            program_path.push("parse_errors_input");
            program_path.push($file_name);

            let file_text = ::pktfmt::file_text::FileText::new(program_path.as_path()).unwrap();
            let tokenizer = ::pktfmt::token::Tokenizer::new(file_text.text());
            let parse_res = ::pktfmt::parse_with_error!($parser, tokenizer);

            let mut out: Vec<u8> = ::std::vec::Vec::new();
            ::pktfmt::utils::render_error(&file_text, parse_res.unwrap_err(), &mut out);
        
            let mut dump_path = std::env::current_dir().unwrap();
            dump_path.push("tests");
            dump_path.push("parse_errors_input");
            dump_path.push(format!("{}.stderr", $file_name));

            let mut f = std::fs::File::create(dump_path).unwrap();
            f.write(&out).unwrap();
        }
    };
}


#[test]
fn field_error_1() {
    test_parse_error!("field_error_1", parser::PacketParser);
}

