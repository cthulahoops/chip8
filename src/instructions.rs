fn high_nibble(x: u8) -> u8 {
    (x & 0xf0) >> 4
}

fn low_nibble(x: u8) -> u8 {
    x & 0x0f
}

fn last_12(a: u8, b: u8) -> usize {
    (low_nibble(a) as usize) << 8 | b as usize
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    Assign = 0x0,
    Or = 0x1,
    And = 0x2,
    Xor = 0x3,
    Add = 0x4,
    Sub = 0x5,
    RightShift = 0x6,
    NegSub = 0x7,
    LeftShift = 0xe,
}

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Panic,
    ClearDisplay,
    Return,
    Jump(usize),
    Call(usize),
    SkipIfEqual(usize, u8),
    SkipIfNotEqual(usize, u8),
    SkipIfEqualV(usize, usize),
    SkipIfNotEqualV(usize, usize),
    StoreConst(usize, u8),
    AddConst(usize, u8),
    StoreI(usize),
    JumpV0(usize),
    StoreRand(usize, u8),
    Operator(usize, usize, Operator),
    Draw(usize, usize, u8),
    SkipIfPressed(usize),
    SkipIfNotPressed(usize),
    GetDelay(usize),
    GetKey(usize),
    SetDelay(usize),
    SetSound(usize),
    AddI(usize),
    LoadSprite(usize),
    StoreBCD(usize),
    DumpRegisters(usize),
    LoadRegisters(usize),
}

impl Instruction {
    pub fn parse(a: u8, b: u8) -> Result<Self, String> {
        let instruction = match high_nibble(a) {
            0x0 => match b {
                0x00 => Instruction::Panic,
                0xe0 => Instruction::ClearDisplay,
                0xee => Instruction::Return,
                _ => return Err("Unimplemented.".to_string()),
            },
            0x1 => Instruction::Jump(last_12(a, b)),
            0x2 => Instruction::Call(last_12(a, b)),
            0x3 => Instruction::SkipIfEqual(low_nibble(a) as usize, b),
            0x4 => Instruction::SkipIfNotEqual(low_nibble(a) as usize, b),
            0x5 => Instruction::SkipIfEqualV(low_nibble(a) as usize, high_nibble(b) as usize),
            0x6 => Instruction::StoreConst(low_nibble(a) as usize, b),
            0x7 => Instruction::AddConst(low_nibble(a) as usize, b),
            0x8 => Instruction::Operator(
                low_nibble(a) as usize,
                high_nibble(b) as usize,
                match low_nibble(b) {
                    0x0 => Operator::Assign,
                    0x1 => Operator::Or,
                    0x2 => Operator::And,
                    0x3 => Operator::Xor,
                    0x4 => Operator::Add,
                    0x5 => Operator::Sub,
                    0x6 => Operator::RightShift,
                    0x7 => Operator::NegSub,
                    0xe => Operator::LeftShift,
                    _ => panic!("Not implemented."),
                },
            ),
            0x9 => Instruction::SkipIfNotEqualV(low_nibble(a) as usize, high_nibble(b) as usize),
            0xa => Instruction::StoreI(last_12(a, b)),
            0xb => Instruction::JumpV0(last_12(a, b)),
            0xc => Instruction::StoreRand(low_nibble(a) as usize, b),
            0xd => Instruction::Draw(
                low_nibble(a) as usize,
                high_nibble(b) as usize,
                low_nibble(b),
            ),
            0xe => {
                let x = low_nibble(a) as usize;
                match b {
                    0x9e => Instruction::SkipIfPressed(x),
                    0xa1 => Instruction::SkipIfNotPressed(x),
                    _ => return Err(format!("Not implemented: F {:02x}", b)),
                }
            }
            0xf => {
                let x = low_nibble(a) as usize;
                match b {
                    0x07 => Instruction::GetDelay(x),
                    0x0a => Instruction::GetKey(x),
                    0x15 => Instruction::SetDelay(x),
                    0x18 => Instruction::SetSound(x),
                    0x1e => Instruction::AddI(x),
                    0x29 => Instruction::LoadSprite(x),
                    0x33 => Instruction::StoreBCD(x),
                    0x55 => Instruction::DumpRegisters(x),
                    0x65 => Instruction::LoadRegisters(x),
                    _ => return Err(format!("Not implemented: F {:02x}", b)),
                }
            }
            _ => return Err(format!("Not implemented: {:#02x} {:#02x}", a, b)),
        };
        Ok(instruction)
    }

    pub fn to_bytes(self) -> (u8, u8) {
        match self {
            Instruction::StoreConst(x, value) => ((0x6 << 4) | x as u8, value),
            Instruction::Operator(x, y, operator) => {
                ((0x8 << 4) | x as u8, ((y as u8) << 4) | operator as u8)
            }
            _ => panic!("Not implemented."),
        }
    }
}
