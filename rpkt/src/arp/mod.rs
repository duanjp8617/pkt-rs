enum_sim! {
    pub struct Hardware (u16) {
        ETHERNET = 1
    }
}

enum_sim! {
    pub struct Operation (u16) {
        REQUEST = 1,
        REPLY = 2
    }
}

header_field_range_accessors! {
    (htype, htype_mut, 0..2),
    (ptype, ptype_mut, 2..4),
    (oper, oper_mut, 6..8),
    (sha, sha_mut, 8..14),
    (spa, spa_mut, 14..18),
    (tha, tha_mut, 18..24),
    (tpa, tpa_mut, 24..28)
}

header_field_val_accessors! {
    (hlen, hlen_mut, 4),
    (plen, plen_mut, 5),
}

#[allow(unused_parens)]
mod header;
pub use header::{ArpHeader, ARP_HEADER_LEN, ARP_HEADER_TEMPLATE};

mod packet;
pub use self::packet::ArpPacket;
