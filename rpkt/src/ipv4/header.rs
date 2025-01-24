use byteorder::{ByteOrder, NetworkEndian};

use super::{IpProtocol, Ipv4Addr};

header_field_val_accessors! {
    (ver_ihl, ver_ihl_mut, 0),
    (dscp_ecn, dscp_ecn_mut, 1),
    (ttl, ttl_mut, 8),
    (protocol, protocol_mut, 9)
}

header_field_range_accessors! {
    (length, length_mut, 2..4),
    (ident, ident_mut, 4..6),
    (flag_fragoff, flag_fragoff_mut, 6..8),
    (checksum, checksum_mut, 10..12),
    (source_ip, source_ip_mut, 12..16),
    (dest_ip, dest_ip_mut, 16..20)
}

pub const IPV4_HEADER_LEN: usize = 20;

/// Maximum length of the TCP header with options
pub const IPV4_HEADER_LEN_MAX: usize = 60;

pub const IPV4_HEADER_TEMPLATE: Ipv4Header<[u8; 20]> = Ipv4Header {
    buf: [
        0x45, 0x00, 0x00, 0x14, 0x00, 0x00, 0x40, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00,
    ],
};

/// protocol Ipv4 {
///   // ip version field
///   version: 4| u8, u8 | 4
///   header_len: Bit(4), Start(4), End(Some(60)), Mult(4) | Repr(u8), Arg(u8) |
/// Default(20),   dscp: Bit(6) | Repr(u8), Arg(u8) | Default(0),
///   ecn: 2 | u8, u8 | 0,
///   packet_len: Bit(16), Start(header_len), End(None), Mult(1) | Repr(u16),
/// Arg(u16) | Default(20),   ident: 16 | u16, u16 | 0,
///   flag_reserved: 1 | u8, u8 | 0
///   flag_dont_frag: 1 | u8, bool | true,
///   flag_more_frag: 1 | u8, bool | false,
///   frag_offset: 13 | u16, u16 | 0,
///   time_to_live: 8 | u8, u8 | 0,
///   protocol: 8 | u8, IpProtocol | IpProtocol::TCP,
///   checksum: 16 | u16, u16 | 0,
///   src_ip: 32 | &[u8], Ipv4Addr | [0;0;0;0],
///   dst_ip: 32 | &[u8], Ipv4Addr | [0;0;0;0],
///   Ipv4Option,
///   Payload,
/// }
#[derive(Clone, Copy, Debug)]
pub struct Ipv4Header<T> {
    buf: T,
}

impl<T: AsRef<[u8]>> Ipv4Header<T> {
    #[inline]
    pub fn new(buf: T) -> Result<Self, T> {
        if buf.as_ref().len() >= IPV4_HEADER_LEN {
            Ok(Self { buf })
        } else {
            Err(buf)
        }
    }

    #[inline]
    pub fn new_unchecked(buf: T) -> Self {
        Self { buf }
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.buf.as_ref()[0..IPV4_HEADER_LEN]
    }

    #[inline]
    pub fn to_owned(&self) -> Ipv4Header<[u8; IPV4_HEADER_LEN]> {
        let mut buf = [0; IPV4_HEADER_LEN];
        buf.copy_from_slice(self.as_bytes());
        Ipv4Header { buf }
    }

    #[inline]
    pub fn check_version(&self) -> bool {
        let data = *ver_ihl(self.buf.as_ref());
        (data >> 4) == 4
    }

    #[inline]
    pub fn header_len(&self) -> u8 {
        let data = *ver_ihl(self.buf.as_ref());
        (data & 0x0f) << 2
    }

    #[inline]
    pub fn dscp(&self) -> u8 {
        let data = *dscp_ecn(self.buf.as_ref());
        data >> 2
    }

    #[inline]
    pub fn ecn(&self) -> u8 {
        let data = *dscp_ecn(self.buf.as_ref());
        data & 0x03
    }

    #[inline]
    pub fn packet_len(&self) -> u16 {
        let data = length(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn ident(&self) -> u16 {
        let data = ident(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn dont_frag(&self) -> bool {
        let data = flag_fragoff(self.buf.as_ref());
        NetworkEndian::read_u16(data) & 0x4000 != 0
    }

    #[inline]
    pub fn more_frags(&self) -> bool {
        let data = flag_fragoff(self.buf.as_ref());
        NetworkEndian::read_u16(data) & 0x2000 != 0
    }

    #[inline]
    pub fn frag_offset(&self) -> u16 {
        let data = flag_fragoff(self.buf.as_ref());
        (NetworkEndian::read_u16(data) & !0xe000) << 3
    }

    #[inline]
    pub fn time_to_live(&self) -> u8 {
        let data = *ttl(self.buf.as_ref());
        data
    }

    #[inline]
    pub fn protocol(&self) -> IpProtocol {
        let data = *protocol(self.buf.as_ref());
        IpProtocol::from(data)
    }

    #[inline]
    pub fn checksum(&self) -> u16 {
        let data = checksum(self.buf.as_ref());
        NetworkEndian::read_u16(data)
    }

    #[inline]
    pub fn source_ip(&self) -> Ipv4Addr {
        let data = source_ip(self.buf.as_ref());
        Ipv4Addr::from_bytes(data)
    }

    #[inline]
    pub fn dest_ip(&self) -> Ipv4Addr {
        let data = dest_ip(self.buf.as_ref());
        Ipv4Addr::from_bytes(data)
    }

    #[inline]
    pub fn parse(buf: T) -> Result<Self, T> {
        let remaining_len = buf.as_ref().len();
        if remaining_len < 20 {
            return Err(buf);
        }
        let container = Self { buf };
        if ((container.header_len() as usize) < 20)
            || ((container.packet_len() as usize) < (container.header_len() as usize))
            || ((container.packet_len() as usize) > remaining_len)
        {
            return Err(container.buf);
        }
        Ok(container)
    }
}

impl<T: AsMut<[u8]>> Ipv4Header<T> {
    #[inline]
    pub fn adjust_version(&mut self) {
        let data = ver_ihl_mut(self.buf.as_mut());
        *data = (*data & !0xf0) | (4 << 4);
    }

    #[inline]
    pub fn set_header_len(&mut self, value: u8) {
        assert!(value >= 20 && value <= 60 && value & 0x03 == 0);
        let data = ver_ihl_mut(self.buf.as_mut());
        *data = (*data & !0x0f) | ((value >> 2) & 0x0f);
    }

    #[inline]
    pub fn set_dscp(&mut self, value: u8) {
        assert!(value < 64, "invalid dscp value: {}", value);
        let data = dscp_ecn_mut(self.buf.as_mut());
        *data = (*data & !0xfc) | (value << 2)
    }

    #[inline]
    pub fn set_ecn(&mut self, value: u8) {
        assert!(value < 4, "invalid ecn value: {}", value);
        let data = dscp_ecn_mut(self.buf.as_mut());
        *data = (*data & !0x03) | (value & 0x03)
    }

    #[inline]
    pub fn set_packet_len(&mut self, value: u16) {
        let data = length_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value);
    }

    #[inline]
    pub fn set_ident(&mut self, value: u16) {
        let data = ident_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }

    #[inline]
    pub fn clear_flags(&mut self) {
        let data = flag_fragoff_mut(self.buf.as_mut());
        let raw = NetworkEndian::read_u16(data) & !0xe000;
        NetworkEndian::write_u16(data, raw);
    }

    #[inline]
    pub fn set_dont_frag(&mut self, value: bool) {
        let data = flag_fragoff_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | 0x4000
        } else {
            NetworkEndian::read_u16(data) & !0x4000
        };
        NetworkEndian::write_u16(data, raw);
    }

    #[inline]
    pub fn set_more_frags(&mut self, value: bool) {
        let data = flag_fragoff_mut(self.buf.as_mut());
        let raw = if value {
            NetworkEndian::read_u16(data) | 0x2000
        } else {
            NetworkEndian::read_u16(data) & !0x2000
        };
        NetworkEndian::write_u16(data, raw);
    }

    #[inline]
    pub fn set_frag_offset(&mut self, value: u16) {
        assert!(value & 0x07 == 0, "invalid fragment offset: {}", value);
        let data = flag_fragoff_mut(self.buf.as_mut());
        let raw = (NetworkEndian::read_u16(data) & 0xe000) | (value >> 3);
        NetworkEndian::write_u16(data, raw);
    }

    #[inline]
    pub fn set_time_to_live(&mut self, value: u8) {
        *ttl_mut(self.buf.as_mut()) = value;
    }

    #[inline]
    pub fn set_protocol(&mut self, value: IpProtocol) {
        *protocol_mut(self.buf.as_mut()) = value.into();
    }

    #[inline]
    pub fn set_checksum(&mut self, value: u16) {
        let data = checksum_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value)
    }

    #[inline]
    pub fn set_source_ip(&mut self, value: Ipv4Addr) {
        let data = source_ip_mut(self.buf.as_mut());
        data.copy_from_slice(value.as_bytes())
    }

    #[inline]
    pub fn set_dest_ip(&mut self, value: Ipv4Addr) {
        let data = dest_ip_mut(self.buf.as_mut());
        data.copy_from_slice(value.as_bytes())
    }
}
