use byteorder::{ByteOrder, NetworkEndian};

use crate::ether::EtherType;

use super::{hlen, htype, oper, plen, ptype, sha, spa, tha, tpa};
use super::{
    hlen_mut, htype_mut, oper_mut, plen_mut, ptype_mut, sha_mut, spa_mut, tha_mut, tpa_mut,
};
use super::{Hardware, Operation};

pub const ARP_HEADER_LEN: usize = 28;

pub const ARP_HEADER_TEMPLATE: ArpHeader<[u8; ARP_HEADER_LEN]> = ArpHeader {
    buf: [
        0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ],
};

/// Fixed length header, can be defined as \
/// // Arp protocol definition
/// protocol Arp(header, packet) {
///    hardware_type: Bit(32) | To(Hardware) | Default(Hardware::Ethernet),
///    protocol_type: Bit<16> | To<EtherType> | Default(EtherType::IPV4),
///    hardware_addr_len: Bit<8> | To<u8> | Default(6), 
///    protocol_addr_lenlen: Bit<8> | To<u8> | Default(4),
///    operation: Bit<16> | To<Operation> | Default(Operation::REQUEST),
///    sender_hardware_addr: Bit<48> | To<&[u8]> | Default([00;00;00;00;00;00]),
///    sender_protocol_addr: Bit<32> | To<u32> | Default(0),
///    target_hardware_addr: Bit<48> | To<&[u8]> | Default([00;00;00;00;00;00]),
///    target_protocol_addr: BiT<32> | To<u32> | Default(0),
/// }
#[derive(Clone, Copy, Debug)]
pub struct ArpHeader<T> {
    buf: T,
}

impl<T: AsRef<[u8]>> ArpHeader<T> {
    #[inline]
    pub fn new(buf: T) -> Result<Self, T> {
        if buf.as_ref().len() >= ARP_HEADER_LEN {
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
        &self.buf.as_ref()[0..ARP_HEADER_LEN]
    }

    #[inline]
    pub fn to_owned(&self) -> ArpHeader<[u8; ARP_HEADER_LEN]> {
        let mut buf = [0; ARP_HEADER_LEN];
        buf.copy_from_slice(self.as_bytes());
        ArpHeader { buf }
    }

    #[inline]
    pub fn hardware_type(&self) -> Hardware {
        let data = htype(self.buf.as_ref());
        NetworkEndian::read_u16(data).into()
    }

    #[inline]
    pub fn protocol_type(&self) -> EtherType {
        let data = ptype(self.buf.as_ref());
        NetworkEndian::read_u16(data).into()
    }

    #[inline]
    pub fn hardware_len(&self) -> u8 {
        *hlen(self.buf.as_ref())
    }

    #[inline]
    pub fn protocol_len(&self) -> u8 {
        *plen(self.buf.as_ref())
    }

    #[inline]
    pub fn operation(&self) -> Operation {
        let data = oper(self.buf.as_ref());
        NetworkEndian::read_u16(data).into()
    }

    #[inline]
    pub fn sender_hardware_addr(&self) -> &[u8] {
        sha(self.buf.as_ref())
    }

    #[inline]
    pub fn sender_protocol_addr(&self) -> &[u8] {
        spa(self.buf.as_ref())
    }

    #[inline]
    pub fn target_hardware_addr(&self) -> &[u8] {
        tha(self.buf.as_ref())
    }

    #[inline]
    pub fn target_protocol_addr(&self) -> &[u8] {
        tpa(self.buf.as_ref())
    }
}

impl<T: AsMut<[u8]>> ArpHeader<T> {
    #[inline]
    pub fn set_hardware_type(&mut self, value: Hardware) {
        let data = htype_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value.into())
    }

    #[inline]
    pub fn set_protocol_type(&mut self, value: EtherType) {
        let data = ptype_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value.into())
    }

    #[inline]
    pub fn set_hardware_len(&mut self, value: u8) {
        *hlen_mut(self.buf.as_mut()) = value;
    }

    #[inline]
    pub fn set_protocol_len(&mut self, value: u8) {
        *plen_mut(self.buf.as_mut()) = value;
    }

    #[inline]
    pub fn set_operation(&mut self, value: Operation) {
        let data = oper_mut(self.buf.as_mut());
        NetworkEndian::write_u16(data, value.into())
    }

    #[inline]
    pub fn set_sender_hardware_addr(&mut self, value: &[u8]) {
        let data = sha_mut(self.buf.as_mut());
        data.copy_from_slice(value);
    }

    #[inline]
    pub fn set_sender_protocol_addr(&mut self, value: &[u8]) {
        let data = spa_mut(self.buf.as_mut());
        data.copy_from_slice(value)
    }

    #[inline]
    pub fn set_target_hardware_addr(&mut self, value: &[u8]) {
        let data = tha_mut(self.buf.as_mut());
        data.copy_from_slice(value)
    }

    #[inline]
    pub fn set_target_protocol_addr(&mut self, value: &[u8]) {
        let data = tpa_mut(self.buf.as_mut());
        data.copy_from_slice(value)
    }
}
