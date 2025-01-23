use bytes::Buf;

use crate::checksum_utils;
use crate::cursors_old::{Cursor, CursorMut};
use crate::ipv4::{Ipv4Addr, Ipv4PseudoHeader};
use crate::{PktBuf, PktMut};

use super::header::{UdpHeader, UDP_HEADER_LEN};

packet_base! {
    pub struct UdpPacket: UdpHeader {
        header_len: UDP_HEADER_LEN,
        get_methods: [
            (source_port, u16),
            (dest_port, u16),
            (packet_len, u16),
            (checksum, u16),
        ],
        set_methods: [
            (set_source_port, val: u16),
            (set_dest_port, val: u16),
            (set_checksum, val: u16),
        ],
        unchecked_set_methods:[
            (set_packet_len_unchecked, set_packet_len, value: u16)
        ]
    }
}

impl<T: Buf> UdpPacket<T> {
    #[inline]
    pub fn parse(buf: T) -> Result<UdpPacket<T>, T> {
        if buf.chunk().len() < UDP_HEADER_LEN {
            return Err(buf);
        }

        let packet = UdpPacket::parse_unchecked(buf);

        if usize::from(packet.packet_len()) >= UDP_HEADER_LEN
            && usize::from(packet.packet_len()) <= packet.buf.remaining()
        {
            Ok(packet)
        } else {
            Err(packet.release())
        }
    }
}

impl<T: PktBuf> UdpPacket<T> {
    pub fn calc_checksum(&mut self) -> u16 {
        let total_len = self.packet_len();

        let result = checksum_utils::from_buf(&mut self.buf, total_len.into());
        self.buf.move_back(total_len.into());
        result
    }

    #[inline]
    pub fn verify_ipv4_checksum(&mut self, src_addr: Ipv4Addr, dst_addr: Ipv4Addr) -> bool {
        if self.checksum() == 0 {
            return true;
        }

        let phdr = Ipv4PseudoHeader::from_udp_pkt(src_addr, dst_addr, self);

        let cksum = checksum_utils::combine(&[phdr.calc_checksum(), self.calc_checksum()]);

        cksum == !0
    }

    #[inline]
    pub fn payload(self) -> T {
        assert!(usize::from(self.packet_len()) <= self.buf.remaining());
        let trim_size = self.buf.remaining() - usize::from(self.packet_len());

        let mut buf = self.release();
        if trim_size > 0 {
            buf.trim_off(trim_size);
        }

        buf.advance(UDP_HEADER_LEN);

        buf
    }
}

impl<T: PktMut> UdpPacket<T> {
    #[inline]
    pub fn adjust_ipv4_checksum(&mut self, src_addr: Ipv4Addr, dst_addr: Ipv4Addr) {
        self.set_checksum(0);

        let phdr = Ipv4PseudoHeader::from_udp_pkt(src_addr, dst_addr, self);

        let cksum = !checksum_utils::combine(&[phdr.calc_checksum(), self.calc_checksum()]);

        // UDP checksum value of 0 means no checksum; if the checksum really is zero,
        // use all-ones, which indicates that the remote end must verify the checksum.
        // Arithmetically, RFC 1071 checksums of all-zeroes and all-ones behave
        // identically, so no action is necessary on the remote end.
        self.set_checksum(if cksum == 0 { 0xffff } else { cksum })
    }

    #[inline]
    pub fn prepend_header<TH: AsRef<[u8]>>(mut buf: T, header: &UdpHeader<TH>) -> UdpPacket<T> {
        assert!(buf.chunk_headroom() >= UDP_HEADER_LEN);
        buf.move_back(UDP_HEADER_LEN);

        let data = &mut buf.chunk_mut()[0..UDP_HEADER_LEN];
        data.copy_from_slice(header.as_bytes());

        let mut udppkt = UdpPacket::parse_unchecked(buf);
        udppkt.set_packet_len_unchecked(u16::try_from(udppkt.buf().remaining()).unwrap());
        udppkt
    }
}

impl<'a> UdpPacket<Cursor<'a>> {
    #[inline]
    pub fn cursor_header(&self) -> UdpHeader<&[u8]> {
        let data = &self.buf.current_buf()[..UDP_HEADER_LEN];
        UdpHeader::new_unchecked(data)
    }

    #[inline]
    pub fn cursor_payload(&self) -> Cursor<'_> {
        Cursor::new(&self.buf.current_buf()[UDP_HEADER_LEN..usize::from(self.packet_len())])
    }
}

impl<'a> UdpPacket<CursorMut<'a>> {
    #[inline]
    pub fn cursor_payload(&mut self) -> CursorMut<'_> {
        let packet_len = usize::from(self.packet_len());
        CursorMut::new(&mut self.buf.current_buf()[UDP_HEADER_LEN..packet_len])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cursors_old::{Cursor, CursorMut};
    use crate::ether::*;
    use crate::ipv4::*;
    use crate::udp::UDP_HEADER_TEMPLATE;

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
        assert_eq!(ippkt.protocol(), IpProtocol::UDP);
        assert_eq!(ippkt.source_ip(), Ipv4Addr([192, 168, 29, 58]));
        assert_eq!(ippkt.dest_ip(), Ipv4Addr([192, 168, 29, 160]));

        let udppkt = UdpPacket::parse(ippkt.payload()).unwrap();
        assert_eq!(udppkt.source_port(), 60376);
        assert_eq!(udppkt.dest_port(), 161);
        assert_eq!(udppkt.packet_len(), 74);
        assert_eq!(udppkt.checksum(), 0xbc86);

        let payload = udppkt.payload();
        assert_eq!(
            payload.chunk(),
            &FRAME_BYTES[ETHER_HEADER_LEN + IPV4_HEADER_LEN + UDP_HEADER_LEN..108]
        );
    }

    #[test]
    fn packet_build() {
        let mut bytes = [0xff; 108];
        (&mut bytes[ETHER_HEADER_LEN + IPV4_HEADER_LEN + UDP_HEADER_LEN..108]).copy_from_slice(
            &FRAME_BYTES[ETHER_HEADER_LEN + IPV4_HEADER_LEN + UDP_HEADER_LEN..108],
        );

        let mut pktbuf = CursorMut::new(&mut bytes[..]);
        pktbuf.advance(ETHER_HEADER_LEN + IPV4_HEADER_LEN + UDP_HEADER_LEN);

        let mut udppkt = UdpPacket::prepend_header(pktbuf, &UDP_HEADER_TEMPLATE);
        udppkt.set_source_port(60376);
        udppkt.set_dest_port(161);
        udppkt.set_checksum(0xbc86);

        let mut ippkt = Ipv4Packet::prepend_header(udppkt.release(), &IPV4_HEADER_TEMPLATE);
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

        let v = ethpkt.release();
        assert_eq!(v.chunk(), &FRAME_BYTES[..108]);
    }

    #[test]
    fn cursor_parse1() {
        let buf = Cursor::new(&FRAME_BYTES[..]);

        let ethpkt = EtherPacket::parse(buf).unwrap();
        let ipv4_pkt = Ipv4Packet::parse(ethpkt.cursor_payload()).unwrap();
        let udp_pkt = UdpPacket::parse(ipv4_pkt.cursor_payload()).unwrap();

        assert_eq!(ethpkt.ethertype(), EtherType::IPV4);
        assert_eq!(ipv4_pkt.protocol(), IpProtocol::UDP);
        assert_eq!(udp_pkt.checksum(), 0xbc86);
    }

    #[test]
    fn cursor_parse2() {
        let buf = Cursor::new(&FRAME_BYTES[..]);

        let ethpkt = EtherPacket::parse(buf).unwrap();
        let eth_hdr = ethpkt.cursor_header();

        let ipv4_pkt = Ipv4Packet::parse(ethpkt.cursor_payload()).unwrap();
        let ipv4_hdr = ipv4_pkt.cursor_header();

        let udp_pkt = UdpPacket::parse(ipv4_pkt.cursor_payload()).unwrap();
        let udp_hdr = udp_pkt.cursor_header();

        assert_eq!(eth_hdr.ethertype(), EtherType::IPV4);
        assert_eq!(ipv4_hdr.protocol(), IpProtocol::UDP);
        assert_eq!(udp_hdr.checksum(), 0xbc86);
    }

    #[test]
    fn fake_nat() {
        let mut buf = [0; 110];
        buf.copy_from_slice(&FRAME_BYTES[..]);

        let pkt = CursorMut::new(&mut buf[..]);

        let mut ethpkt = EtherPacket::parse(pkt).unwrap();
        let payload = ethpkt.cursor_payload();

        let mut ippkt = Ipv4Packet::parse(payload).unwrap();
        let ip_header_len = usize::from(ippkt.header_len());
        let ip_hdr_cursor = ippkt.buf().cursor() + 14;

        let payload = ippkt.cursor_payload();
        let udp_hdr_cursor = ip_hdr_cursor + ip_header_len;

        let mut udppkt = UdpPacket::parse(payload).unwrap();
        let _ = udppkt.cursor_payload();

        udppkt.set_source_port(1024);
        ippkt.set_source_ip(Ipv4Addr([127, 0, 0, 1]));

        assert_eq!(ip_hdr_cursor, 14);
        assert_eq!(udp_hdr_cursor, 34);

        let pkt = Cursor::new(&buf[..]);
        let ethpkt = EtherPacket::parse(pkt).unwrap();

        let ippkt = Ipv4Packet::parse(ethpkt.payload()).unwrap();
        assert_eq!(ippkt.source_ip(), Ipv4Addr([127, 0, 0, 1]));

        let udppkt = UdpPacket::parse(ippkt.payload()).unwrap();
        assert_eq!(udppkt.source_port(), 1024);
    }
}
