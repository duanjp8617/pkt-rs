use pktfmt::*;
use std::env;

fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        eprintln!(
            "Currently we only accept a single argument, which is the name of the input file."
        );
        std::process::exit(1);
    }
    args.next();

    // lexical analysis
    let file_text = file_text::FileText::new(args.next().unwrap()).unwrap();
    let tokenizer = token::Tokenizer::new(file_text.text());

    // parsing for the abstract syntax tree
    let ast = match parse_with_error!(parser::PacketParser, tokenizer) {
    // let ast = match parse_with_error!(parser::MessageParser, tokenizer) {
        Ok(ast) => ast,
        Err(err) => {
            let mut stderr = std::io::stderr();
            utils::render_error(&file_text, err, &mut stderr);
            std::process::exit(1)
        }
    };

    // codegen to a writable buffer
    let mut buf: Vec<u8> = Vec::new();

    // let header = codegen::HeaderGen::new(&ast);
    // header.code_gen(&mut buf);
    // let packet = codegen::PacketGen::new(&ast);
    // packet.code_gen(&mut buf);

    // let message = codegen::MessageGen::new(&ast);
    // message.code_gen(&mut buf);

    let packet = codegen::PacketGenForContiguousBuf::new(&ast);
    packet.code_gen(&mut buf);

    println!("{}", std::str::from_utf8(&buf[..]).unwrap());
}
