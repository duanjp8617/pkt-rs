use pktfmt::*;

#[macro_use]
mod common;

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

    print_parse_error!(fname_ut, err_reason, pktfmt::parser::FieldParser);
}
