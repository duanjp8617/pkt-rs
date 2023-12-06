use bytes::Buf;

use crate::PktMut;
use crate::{Cursor, CursorMut};

use super::header::{EtherHeader, ETHER_HEADER_LEN};
use super::{EtherType, MacAddr};

/// The default ethernet overhead without VLAN.
/// It includes the 14-byte ethernet header and the 4-byte crc checksum.
pub const ETHER_OVERHEAD: usize = 14 + 4;

/// The minimum ethernet frame size is 64.
pub const ETHER_MIN_LEN: usize = 64;

/// The maximum ethernet frame size is 1518
pub const ETHER_MAX_LEN: usize = 1518;

/// The default ethernet mtu value.
pub const ETHER_MTU: usize = 1500;

/// The maximum frame size of an ethernet jumboframe.
pub const ETHER_MAX_JUMBO_PKT_LEN: usize = 9600;

packet_base! {
    /// # Default Header Format
    /// `src_port`: 0,
    ///
    /// `dst_port`: 0,
    ///
    /// `checksum`: 0x0000,
    ///
    /// `total_len`: 0
    pub struct EtherPacket: EtherHeader {
        header_len: ETHER_HEADER_LEN,
        get_methods: [
            /// fuck
            (dest_mac, MacAddr),
            /// fuck
            (source_mac, MacAddr),
            /// fuck
            (ethertype, EtherType)
        ],
        set_methods: [
            /// fuck
            (set_dest_mac, value: MacAddr),
            /// fuck
            (set_source_mac, value: MacAddr),
            /// fuck
            (set_ethertype, value: EtherType)
        ],
        unchecked_set_methods: []
    }
}

impl<T: Buf> EtherPacket<T> {
    #[inline]
    pub fn parse(buf: T) -> Result<EtherPacket<T>, T> {
        if buf.chunk().len() >= ETHER_HEADER_LEN {
            Ok(EtherPacket::parse_unchecked(buf))
        } else {
            Err(buf)
        }
    }

    #[inline]
    pub fn payload(self) -> T {
        assert!(
            ETHER_HEADER_LEN <= self.buf.remaining(),
            "cannot obtain the ethernet payload, please check packet buffer format"
        );

        let mut buf = self.release();
        buf.advance(ETHER_HEADER_LEN);

        buf
    }
}

impl<T: PktMut> EtherPacket<T> {
    #[inline]
    pub fn prepend_header<HT: AsRef<[u8]>>(mut buf: T, header: &EtherHeader<HT>) -> EtherPacket<T> {
        assert!(
            buf.chunk_headroom() >= ETHER_HEADER_LEN,
            "cannot prepend the ethernet header, please check argument formats"
        );
        buf.move_back(ETHER_HEADER_LEN);

        let data = &mut buf.chunk_mut()[0..ETHER_HEADER_LEN];
        data.copy_from_slice(header.as_bytes());

        EtherPacket { buf }
    }
}

impl<'a> EtherPacket<Cursor<'a>> {
    #[inline]
    pub fn cursor_header(&self) -> EtherHeader<&'a [u8]> {
        let data = &self.buf.chunk_shared_lifetime()[..ETHER_HEADER_LEN];
        EtherHeader::new_unchecked(data)
    }

    #[inline]
    pub fn cursor_payload(&self) -> Cursor<'a> {
        Cursor::new(&self.buf.chunk_shared_lifetime()[ETHER_HEADER_LEN..])
    }
}

impl<'a> EtherPacket<CursorMut<'a>> {
    #[inline]
    pub fn split(self) -> (EtherHeader<&'a mut [u8]>, CursorMut<'a>) {
        let buf_mut = self.buf.chunk_mut_shared_lifetime();
        let (hdr, payload) = buf_mut.split_at_mut(ETHER_HEADER_LEN);
        (EtherHeader::new_unchecked(hdr), CursorMut::new(payload))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ether::ETHER_HEADER_TEMPLATE, Cursor, CursorMut};
    use bytes::BufMut;

    static FRAME_BYTES: [u8; 64] = [
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x08, 0x00, 0xaa,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0xff,
    ];

    #[test]
    fn packet_parse() {
        let pres = EtherPacket::parse(Cursor::new(&FRAME_BYTES[..]));
        assert_eq!(pres.is_ok(), true);
        let ethpkt = pres.unwrap();
        assert_eq!(
            ethpkt.dest_mac(),
            MacAddr([0x01, 0x02, 0x03, 0x04, 0x05, 0x06])
        );
        assert_eq!(
            ethpkt.source_mac(),
            MacAddr([0x11, 0x12, 0x13, 0x14, 0x15, 0x16])
        );
        assert_eq!(ethpkt.ethertype(), EtherType::IPV4);

        let next = ethpkt.payload();
        assert_eq!(next.chunk(), &FRAME_BYTES[ETHER_HEADER_LEN..]);
    }

    #[test]
    fn packet_build() {
        let mut bytes = [0xff; 64];
        (&mut bytes[ETHER_HEADER_LEN..]).put(&FRAME_BYTES[ETHER_HEADER_LEN..]);

        let mut buf = CursorMut::new(&mut bytes[..]);
        buf.advance(ETHER_HEADER_LEN);

        let mut ethpkt = EtherPacket::prepend_header(buf, &ETHER_HEADER_TEMPLATE);
        ethpkt.set_dest_mac(MacAddr([0x01, 0x02, 0x03, 0x04, 0x05, 0x06]));
        ethpkt.set_source_mac(MacAddr([0x11, 0x12, 0x13, 0x14, 0x15, 0x16]));
        ethpkt.set_ethertype(EtherType::IPV4);

        assert_eq!(ethpkt.buf().chunk(), &FRAME_BYTES[..]);
    }
}
