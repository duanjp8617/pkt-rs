use bytes::Buf;

use crate::checksum_utils;
use crate::cursors_old::{Cursor, CursorMut};
use crate::{PktBuf, PktMut};

use super::header::{Ipv4Header, IPV4_HEADER_LEN};
use super::{IpProtocol, Ipv4Addr};

packet_base! {
    pub struct Ipv4Packet: Ipv4Header {
        header_len: IPV4_HEADER_LEN,
        get_methods: [
            (check_version, bool),
            (header_len, u8),
            (dscp, u8),
            (ecn, u8),
            (packet_len, u16),
            (ident, u16),
            (dont_frag, bool),
            (more_frags, bool),
            (frag_offset, u16),
            (time_to_live, u8),
            (protocol, IpProtocol),
            (checksum, u16),
            (source_ip, Ipv4Addr),
            (dest_ip, Ipv4Addr),
        ],
        set_methods: [
            (adjust_version),
            (set_dscp, value: u8),
            (set_ecn, value: u8),
            (set_ident, value: u16),
            (clear_flags),
            (set_dont_frag, value: bool),
            (set_more_frags, value: bool),
            (set_frag_offset, value: u16),
            (set_time_to_live, value: u8),
            (set_protocol, value: IpProtocol),
            (set_checksum, value: u16),
            (set_source_ip, value: Ipv4Addr),
            (set_dest_ip, value: Ipv4Addr),
        ],
        unchecked_set_methods:[
            (set_header_len_unchecked, set_header_len, value: u8),
            (set_packet_len_unchecked, set_packet_len, value: u16)
        ]
    }
}

impl<T: Buf> Ipv4Packet<T> {
    #[inline]
    pub fn parse(buf: T) -> Result<Ipv4Packet<T>, T> {
        if buf.chunk().len() < IPV4_HEADER_LEN {
            return Err(buf);
        }

        let packet = Ipv4Packet::parse_unchecked(buf);
        if usize::from(packet.header_len()) >= IPV4_HEADER_LEN
            && usize::from(packet.header_len()) <= usize::from(packet.packet_len())
            && usize::from(packet.header_len()) <= packet.buf.chunk().len()
            && usize::from(packet.packet_len()) <= packet.buf.remaining()
        {
            Ok(packet)
        } else {
            Err(packet.release())
        }
    }

    #[inline]
    pub fn option_bytes(&self) -> &[u8] {
        &self.buf.chunk()[IPV4_HEADER_LEN..self.header_len().into()]
    }

    #[inline]
    pub fn calc_checksum(&self) -> u16 {
        let data = &self.buf.chunk()[0..self.header_len().into()];
        checksum_utils::from_slice(data)
    }

    #[inline]
    pub fn verify_checksum(&self) -> bool {
        self.calc_checksum() == !0
    }
}

impl<T: PktBuf> Ipv4Packet<T> {
    #[inline]
    pub fn payload(self) -> T {
        assert!(usize::from(self.packet_len()) <= self.buf.remaining());
        let trim_size = self.buf.remaining() - usize::from(self.packet_len());
        let header_len = usize::from(self.header_len());

        let mut buf = self.release();
        if trim_size > 0 {
            buf.trim_off(trim_size);
        }

        buf.advance(header_len);

        buf
    }
}

impl<T: PktMut> Ipv4Packet<T> {
    #[inline]
    pub fn option_bytes_mut(&mut self) -> &mut [u8] {
        let header_len = self.header_len();
        &mut self.buf.chunk_mut()[IPV4_HEADER_LEN..header_len as usize]
    }

    pub fn adjust_checksum(&mut self) {
        self.set_checksum(0);
        let checksum = !self.calc_checksum();
        self.set_checksum(checksum)
    }

    #[inline]
    pub fn prepend_header<HT: AsRef<[u8]>>(mut buf: T, header: &Ipv4Header<HT>) -> Ipv4Packet<T> {
        let header_len: usize = header.header_len().into();
        assert!(header_len >= IPV4_HEADER_LEN && header_len <= buf.chunk_headroom());

        buf.move_back(header_len);

        let data = &mut buf.chunk_mut()[0..IPV4_HEADER_LEN];
        data.copy_from_slice(header.as_bytes());

        let mut ippkt = Ipv4Packet::parse_unchecked(buf);
        ippkt.set_packet_len_unchecked(u16::try_from(ippkt.buf().remaining()).unwrap());
        ippkt
    }
}

impl<'a> Ipv4Packet<Cursor<'a>> {
    #[inline]
    pub fn cursor_header(&self) -> Ipv4Header<&[u8]> {
        let data = &self.buf.chunk()[..IPV4_HEADER_LEN];
        Ipv4Header::new_unchecked(data)
    }

    #[inline]
    pub fn cursor_options(&self) -> &[u8] {
        &&self.buf.chunk()[IPV4_HEADER_LEN..usize::from(self.header_len())]
    }

    #[inline]
    pub fn cursor_payload(&self) -> Cursor<'_> {
        Cursor::new(
            &self.buf.chunk()[usize::from(self.header_len())..usize::from(self.packet_len())],
        )
    }
}

impl<'a> Ipv4Packet<CursorMut<'a>> {
    #[inline]
    pub fn cursor_payload(&mut self) -> CursorMut<'_> {
        let header_len = usize::from(self.header_len());
        let packet_len = usize::from(self.packet_len());
        CursorMut::new(&mut self.buf.chunk_mut()[header_len..packet_len])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ether::*;
    use crate::ipv4::IPV4_HEADER_TEMPLATE;
    use crate::{Cursor, CursorMut};
    use bytes::BufMut;

    static FRAME_BYTES: [u8; 110] = [
        0x00, 0x0b, 0x86, 0x64, 0x8b, 0xa0, 0x00, 0x50, 0x56, 0xae, 0x76, 0xf5, 0x08, 0x00, 0x45,
        0x00, 0x00, 0x5e, 0x5c, 0x65, 0x00, 0x00, 0x80, 0x11, 0x00, 0x00, 0xc0, 0xa8, 0x1d, 0x3a,
        0xc0, 0xa8, 0x1d, 0xa0, 0xeb, 0xd8, 0x00, 0xa1, 0x00, 0x4a, 0xbc, 0x86, 0x30, 0x40, 0x02,
        0x01, 0x03, 0x30, 0x0f, 0x02, 0x03, 0x00, 0x91, 0xc8, 0x02, 0x02, 0x05, 0xdc, 0x04, 0x01,
        0x04, 0x02, 0x01, 0x03, 0x04, 0x15, 0x30, 0x13, 0x04, 0x00, 0x02, 0x01, 0x00, 0x02, 0x01,
        0x00, 0x04, 0x05, 0x61, 0x64, 0x6d, 0x69, 0x6e, 0x04, 0x00, 0x04, 0x00, 0x30, 0x13, 0x04,
        0x00, 0x04, 0x00, 0xa0, 0x0d, 0x02, 0x03, 0x00, 0x91, 0xc8, 0x02, 0x01, 0x00, 0x02, 0x01,
        0x00, 0x30, 0x00, 0x00, 0x00,
    ];

    #[test]
    fn packet_parse() {
        let buf = Cursor::new(&FRAME_BYTES[..]);

        let ethpkt = EtherPacket::parse(buf).unwrap();
        assert_eq!(ethpkt.ethertype(), EtherType::IPV4);

        let ippkt = Ipv4Packet::parse(ethpkt.payload()).unwrap();
        assert_eq!(ippkt.check_version(), true);
        assert_eq!(ippkt.header_len(), 20);
        assert_eq!(ippkt.dscp(), 0);
        assert_eq!(ippkt.ecn(), 0);
        assert_eq!(ippkt.packet_len(), 94);
        assert_eq!(ippkt.buf().remaining(), 96);
        assert_eq!(ippkt.ident(), 0x5c65);
        assert_eq!(ippkt.dont_frag(), false);
        assert_eq!(ippkt.more_frags(), false);
        assert_eq!(ippkt.frag_offset(), 0);
        assert_eq!(ippkt.time_to_live(), 128);
        assert_eq!(ippkt.protocol(), IpProtocol::UDP);
        assert_eq!(ippkt.checksum(), 0x0000);
        assert_eq!(ippkt.source_ip(), Ipv4Addr([192, 168, 29, 58]));
        assert_eq!(ippkt.dest_ip(), Ipv4Addr([192, 168, 29, 160]));

        let payload = ippkt.payload();
        assert_eq!(
            payload.chunk(),
            &FRAME_BYTES[ETHER_HEADER_LEN + IPV4_HEADER_LEN..108]
        );
    }

    #[test]
    fn packet_build() {
        let mut bytes = [0xff; 110];
        (&mut bytes[(ETHER_HEADER_LEN + IPV4_HEADER_LEN)..])
            .put(&FRAME_BYTES[(ETHER_HEADER_LEN + IPV4_HEADER_LEN)..]);
        let mut buf = CursorMut::new(&mut bytes[..108]);
        buf.advance(ETHER_HEADER_LEN + IPV4_HEADER_LEN);

        let mut ippkt = Ipv4Packet::prepend_header(buf, &IPV4_HEADER_TEMPLATE);
        ippkt.adjust_version();
        ippkt.set_dscp(0);
        ippkt.set_ecn(0);
        ippkt.set_ident(0x5c65);
        ippkt.clear_flags();
        ippkt.set_dont_frag(false);
        ippkt.set_more_frags(false);
        ippkt.set_frag_offset(0);
        ippkt.set_time_to_live(128);
        ippkt.set_protocol(IpProtocol::UDP);
        ippkt.set_checksum(0x0000);
        ippkt.set_source_ip(Ipv4Addr([192, 168, 29, 58]));
        ippkt.set_dest_ip(Ipv4Addr([192, 168, 29, 160]));

        let mut ethpkt = EtherPacket::prepend_header(ippkt.release(), &ETHER_HEADER_TEMPLATE);
        ethpkt.set_dest_mac(MacAddr([0x00, 0x0b, 0x86, 0x64, 0x8b, 0xa0]));
        ethpkt.set_source_mac(MacAddr([0x00, 0x50, 0x56, 0xae, 0x76, 0xf5]));
        ethpkt.set_ethertype(EtherType::IPV4);

        assert_eq!(ethpkt.buf().chunk(), &FRAME_BYTES[..108]);
    }
}
