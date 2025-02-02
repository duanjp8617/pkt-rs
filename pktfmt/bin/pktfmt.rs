use std::env;
use std::fs::File;
use std::io::Write;

use pktfmt::{ast, codegen, file_text, parser, token, utils};

fn driver(file_text: &file_text::FileText, output_file: &str) -> Result<(), utils::Error> {
    let tokenizer = token::Tokenizer::new(file_text.text());
    let (start_code, parsed_items, end_code_opt) = parser::TopLevelParser::new()
        .parse(
            tokenizer
                .into_iter()
                .map(|tk_res| tk_res.map_err(|err| utils::Error::Token(err))),
        )
        .map_err(|err| match err {
            lalrpop_util::ParseError::User { error } => error,
            _ => utils::Error::Lalrpop(format!("{err}")),
        })?;
    let top_level = ast::TopLevel::new(&parsed_items[..])
        .map_err(|(err, span)| utils::Error::Ast { err, span })?;

    // Prepare the output channel.
    let mut output_f = File::create(output_file)
        .map_err(|err| utils::Error::ErrStr(format!("{output_file}: {err}")))?;

    // Do the code generation.
    write!(&mut output_f, "{start_code}").unwrap();
    writeln!(&mut output_f, "").unwrap();
    for parsed_item in top_level.item_iter() {
        match parsed_item {
            ast::ParsedItem::Packet_(p) => {
                let header = codegen::HeaderGen::new(&p);
                header.code_gen(&mut output_f);
                let packet = codegen::PacketGen::new(&p);
                packet.code_gen(&mut output_f);
            }
            ast::ParsedItem::Message_(m) => {
                let message = codegen::MessageGen::new(m);
                message.code_gen(&mut output_f);
            }
            ast::ParsedItem::MessageGroupName_(mg) => {
                let defined_name = mg.name();
                let msgs = top_level.msg_group(defined_name).unwrap();
                let message_group = codegen::GroupMessageGen::new(defined_name, msgs);
                message_group.code_gen(&mut output_f);
            }
        }
        writeln!(&mut output_f, "").unwrap();
    }
    end_code_opt.map(|end_code| write!(&mut output_f, "{end_code}").unwrap());

    Ok(())
}

fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        eprintln!(
            "Currently we only accept a single argument, which is the name of the input file."
        );
        std::process::exit(1);
    }
    args.next();

    let input_file = args.next().unwrap();

    // Prepare the file text.
    let file_text = match file_text::FileText::new(input_file) {
        Ok(file_text) => file_text,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1)
        }
    };

    match driver(&file_text, "./tmp.rs") {
        Err(err) => {
            let mut stderr = std::io::stderr();
            utils::render_error(&file_text, err, &mut stderr);
            std::process::exit(1)
        }
        Ok(_) => {}
    }
}
