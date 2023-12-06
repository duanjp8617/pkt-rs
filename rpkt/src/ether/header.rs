use byteorder::{ByteOrder, NetworkEndian};

use super::{EtherType, MacAddr};

header_field_range_accessors! {
    (dest_mac, dest_mac_mut, 0..6),
    (source_mac, source_mac_mut, 6..12),
    (ethertype, ethertype_mut, 12..14)
}

pub const ETHER_HEADER_LEN: usize = 14;

pub const ETHER_HEADER_TEMPLATE: EtherHeader<[u8; 14]> = EtherHeader {
    buf: [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00,
    ],
};

///
/// protocol Arp(header, packet) {
///    dest_mac_addr: 48,&[u8] | MacAddr | [00;00;00;00;00;00]
///    src_mac_addr: 48,&[u8] | MacAddr | [00;00;00;00;00;00]
///    ether_type: 16,u16 | EtherType | EtherType::IPV4 
/// }
#[derive(Clone, Copy, Debug)]
pub struct EtherHeader<T> {
    buf: T,
}

impl<T: AsRef<[u8]>> EtherHeader<T> {
    #[inline]
    pub fn new(buf: T) -> Result<Self, T> {
        if buf.as_ref().len() >= ETHER_HEADER_LEN {
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
        &self.buf.as_ref()[0..ETHER_HEADER_LEN]
    }

    #[inline]
    pub fn to_owned(&self) -> EtherHeader<[u8; ETHER_HEADER_LEN]> {
        let mut buf = [0; ETHER_HEADER_LEN];
        buf.copy_from_slice(self.as_bytes());
        EtherHeader { buf }
    }

    #[inline]
    pub fn dest_mac(&self) -> MacAddr {
        let data = dest_mac(self.buf.as_ref());
        MacAddr::from_bytes(data)
    }

    #[inline]
    pub fn source_mac(&self) -> MacAddr {
        let data = source_mac(self.buf.as_ref());
        MacAddr::from_bytes(data)
    }

    #[inline]
    pub fn ethertype(&self) -> EtherType {
        let data = ethertype(self.buf.as_ref());
        NetworkEndian::read_u16(data).into()
    }
}

impl<T: AsMut<[u8]>> EtherHeader<T> {
    #[inline]
    pub fn set_dest_mac(&mut self, value: MacAddr) {
        let data = dest_mac_mut(self.buf.as_mut());
        data.copy_from_slice(value.as_bytes())
    }

    #[inline]
    pub fn set_source_mac(&mut self, value: MacAddr) {
        let data = source_mac_mut(self.buf.as_mut());
        data.copy_from_slice(value.as_bytes())
    }

    #[inline]
    pub fn set_ethertype(&mut self, value: EtherType) {
        let data = ethertype_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value.into())
    }
}
