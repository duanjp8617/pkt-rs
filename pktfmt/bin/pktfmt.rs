use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use pktfmt::{ast, codegen, file_text, parser, token, utils};

// A helper that will write an "error" line to the output channel.
fn preceed_with_error(output: &mut dyn Write) -> &mut dyn Write {
    writeln!(output, "error").unwrap();
    output
}

fn handle_args(args: &Vec<String>) -> Result<(PathBuf, PathBuf), String> {
    let mut input_path = None;
    let mut output_path = None;

    let help = r#"Usage: pktfmt <*.pktfmt> [options]
Options:
  -o <file>   Generate the output in <file>.
  -h          Display help information.
"#;

    let mut i = 1;
    while i < args.len() {
        if (&args[i]).ends_with(".pktfmt") && args[i].len() > ".pktfmt".len() {
            // Find a ".pktfmt" suffix, which should be an input file
            input_path = Some(&args[i]);
            i += 1;
        } else if &args[i] == "-o" && (i + 1 < args.len()) {
            output_path = Some(&args[i + 1]);
            i += 2;
        } else if &args[i] == "-h" {
            print!("{help}");
            std::process::exit(0);
        } else {
            i += 1
        }
    }

    match (input_path, output_path) {
        (Some(i), Some(o)) => Ok((PathBuf::from(i), PathBuf::from(o))),
        (Some(i), None) => {
            let end_idx = i.rfind(".pktfmt").unwrap();
            let start_idx = i.rfind("/").unwrap_or(0);
            if start_idx == end_idx {
                Err(format!("invalid input file name: {i}"))
            } else {
                Ok((
                    PathBuf::from(i),
                    std::env::current_dir()
                        .unwrap()
                        .join(PathBuf::from(format!("{}.rs", &i[start_idx..end_idx]))),
                ))
            }
        }
        _ => Err(format!("missing input file")),
    }
}

// The driver function that runs the code generation pipeline.
fn driver(file_text: &file_text::FileText, output_file: &PathBuf) -> Result<(), utils::Error> {
    // Parse for the top level ast.
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
        .map_err(|err| utils::Error::ErrStr(format!("{}: {err}", output_file.to_str().unwrap())))?;

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
    let args: Vec<String> = env::args().into_iter().collect();
    let mut stderr = std::io::stderr();

    // Handle the input args
    let (input_file, output_file) = match handle_args(&args) {
        Ok(res) => res,
        Err(err_str) => {
            writeln!(preceed_with_error(&mut stderr), "{err_str}").unwrap();
            std::process::exit(1);
        }
    };

    // Prepare the file text.
    let file_text = match file_text::FileText::new(input_file) {
        Ok(file_text) => file_text,
        Err(e) => {
            writeln!(preceed_with_error(&mut stderr), "{e}").unwrap();
            std::process::exit(1);
        }
    };

    // Run the code generation driver.
    match driver(&file_text, &output_file) {
        Err(err) => {
            utils::render_error(&file_text, err, &mut stderr);
            std::process::exit(1)
        }
        Ok(_) => {}
    }
}
