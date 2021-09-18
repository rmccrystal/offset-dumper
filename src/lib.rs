mod util;

pub use iced_x86;

use thiserror::Error;
use crate::util::{find_pattern, find_pattern_n, format_instruction};
use iced_x86::*;
use either::Either;

/// A structure that defines to the dumper how to retrieve an offset
#[derive(Clone)]
pub struct OffsetDefinition<'a, 'b> {
    /// List of sigs that the offset can be found with
    pub sigs: &'a [&'b str],
    /// The index of the signatures that are found, -1 being last
    pub index: i32,
    /// How to find the instruction from the sig location
    pub find_type: OffsetFindType,
    /// How to get a offset for the instruction
    pub value_type: OffsetValueType,
}

impl<'a, 'b> OffsetDefinition<'a, 'b> {
    pub const fn new(sigs: &'a [&'b str], index: i32, value_type: OffsetValueType) -> Self {
        Self {
            sigs,
            index,
            find_type: OffsetFindType::None,
            value_type,
        }
    }

    pub const fn new_find(sigs: &'a [&'b str], index: i32, value_type: OffsetValueType, find_type: OffsetFindType) -> Self {
        Self {
            sigs,
            index,
            find_type,
            value_type,
        }
    }
}

/// An error returned when an offset could not be retrieved
#[derive(Error, Debug)]
pub enum OffsetError {
    #[error("signature not found: {0}")]
    SignatureNotFound(String),
    #[error("instruction not found")]
    InstructionNotFound,
    #[error("invalid instruction: {0}")]
    InvalidInstruction(String),
}

/// A enum indicating how to get an offset from a successful sig search
#[derive(Clone)]
pub enum OffsetValueType {
    /// Instruction reads relative to the game's base address.
    /// Example: `lea r11, dword_17C29CB0` will return 0x17C29CB0
    Absolute,
    /// Instruction reads value relative to another register.
    /// Example: `lea rbx, [rcx+528h]` will return 0x528
    Relative,
    /// Instruction has an immediate value
    /// Example: `mul rax 142h` will return 0x142
    Immediate,
    /// Custom function that gets an offset from a register
    Func(fn(&iced_x86::Instruction) -> u64),
}

/// An enum indicating how to find the wanted instruction from the signature location
#[derive(Clone)]
pub enum OffsetFindType {
    None,
    Mnemonic(iced_x86::Mnemonic),
    MemoryBase(iced_x86::Register),
    /// A function that takes an iced decoder and returns an optional instruction
    Func(fn(&iced_x86::Decoder) -> Option<Instruction>),
}

impl OffsetDefinition<'_, '_> {
    /// Gets an offset from the offset definition and the game's memory
    pub fn get(&self, mem: &[u8]) -> Result<u64, OffsetError> {
        let sig = self.find_sig(mem).ok_or_else(|| OffsetError::SignatureNotFound(self.sigs.join("|")))?;
        let instruction = self.find_instruction(mem, sig).ok_or(OffsetError::InstructionNotFound)?;
        match self.get_offset(&instruction) {
            0 => Err(OffsetError::InvalidInstruction(format_instruction(&instruction))),
            n => Ok(n)
        }
    }

    fn find_sig(&self, mem: &[u8]) -> Option<u64> {
        self.sigs.iter().filter_map(|sig| find_pattern_n(mem, sig, self.index)).next()
    }

    fn find_instruction(&self, mem: &[u8], sig: u64) -> Option<Instruction> {
        let mut decoder = Decoder::new(64, mem, DecoderOptions::NONE);
        decoder.try_set_position(sig as _).expect("Could not set position");
        decoder.set_ip(sig);
        match self.find_type {
            OffsetFindType::None => Some(decoder.decode()),
            OffsetFindType::Mnemonic(mne) => decoder.iter().take(2048).find(|i| i.mnemonic() == mne),
            OffsetFindType::MemoryBase(reg) => decoder.iter().take(2048).find(|i| i.memory_base() == reg),
            OffsetFindType::Func(f) => f(&decoder)
        }
    }

    fn get_offset(&self, i: &Instruction) -> u64 {
        match self.value_type {
            OffsetValueType::Absolute => i.memory_displacement64(),
            OffsetValueType::Relative => i.memory_displacement64(),
            OffsetValueType::Immediate => i.immediate64(),
            OffsetValueType::Func(f) => f(i)
        }
    }
}

#[derive(Clone)]
pub struct NamedOffset<'a, 'b, 'c> {
    pub name: &'a str,
    pub namespace: Option<&'a str>,
    /// Either a definition or a constant value
    pub def: Either<OffsetDefinition<'b, 'c>, u64>,
}

impl<'a, 'b, 'c> NamedOffset<'a, 'b, 'c> {
    pub const fn new(name: &'a str, namespace: Option<&'a str>, def: OffsetDefinition<'b, 'c>) -> Self {
        Self { name, namespace, def: Either::Left(def) }
    }

    pub const fn new_const(name: &'a str, namespace: Option<&'a str>, val: u64) -> Self {
        Self { name, namespace, def: Either::Right(val) }
    }
}

pub fn dump_offsets_cpp<'a, 'b, 'c>(offsets: &[NamedOffset<'a, 'b, 'c>], data: &[u8]) -> String {
    let mut buf = String::new();

    let mut namespace = None::<&'a str>;

    for offset in offsets {
        if namespace != offset.namespace {
            if namespace.is_some() {
                buf.push_str("}\n");
            }
            buf.push_str(&match offset.namespace {
                Some(namespace) => format!("\nnamespace {} \n{{\n", namespace),
                None => "\n".to_string()
            });
            namespace = offset.namespace;
        }

        let val = match &offset.def {
            Either::Left(def) => def.get(data),
            Either::Right(n) => Ok(*n)
        };
        buf.push_str(&format!("{}constexpr auto {} = {:#X};", if namespace.is_some() { "\t" } else { "" }, offset.name, val.as_ref().unwrap_or(&0)));
        println!("{}{} = {:#X}", offset.namespace.map(|n| n.to_string() + "_").unwrap_or_else(String::new), offset.name, val.as_ref().unwrap_or(&0));
        if let Err(e) = val.as_ref() {
            buf.push_str(&format!(" // {}", e))
        }
        buf.push('\n');
    }

    if namespace.is_some() {
        buf.push_str("}");
    }

    buf
}