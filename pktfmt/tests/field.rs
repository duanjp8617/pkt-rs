use pktfmt::*;

#[test]
fn field_e1() {
    let fname_ut = "field_e1.pktfmt";
    let err_reason = r#"at /home/djp/pkt-rs/pktcc/pktfmts/field_e1.pktfmt 2:1-5:1
    ~~~~~~~
2 | Field {
3 |     bit = 8, 
4 |     repr = u16,
5 | }
    ^
invalid Field definition: invalid repr
If repr is defined, it must satisfy one of the following conditions:
1. The defined repr is the same as the repr inferred from the bit.
2. The defined repr is a byte slice, and bit % 8 == 0
"#;

    let mut work_dir = std::env::current_dir().unwrap();
    work_dir.push("pktfmts");
    work_dir.push(fname_ut);

    let fut = file_text::FileText::new(work_dir.as_path()).unwrap();
    let tokenizer = token::Tokenizer::new(fut.text());
    let parse_res = parse_with_error!(parser::FieldParser, tokenizer, &fut);

    let mut out: Vec<u8> = Vec::new();
    utils::render_error(&fut, parse_res.unwrap_err(), &mut out);

    assert_eq!(std::str::from_utf8(&out[..]).unwrap(), err_reason);
}