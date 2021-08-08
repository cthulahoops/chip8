use rand::prelude::*;
use std::fs;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::Duration;

struct Stack {
    values: [usize; 0x10],
    top: usize,
}

impl Stack {
    fn new() -> Self {
        Stack {
            values: [0; 0x10],
            top: 0,
        }
    }

    fn push(&mut self, value: usize) {
        self.values[self.top] = value;
        self.top += 1;
    }

    fn pop(&mut self) -> Option<usize> {
        if self.top == 0 {
            return None;
        }
        self.top -= 1;
        let result = self.values[self.top];
        Some(result)
    }
}

fn high_nibble(x: u8) -> u8 {
    (x & 0xf0) >> 4
}

fn low_nibble(x: u8) -> u8 {
    x & 0x0f
}

fn last_12(a: u8, b: u8) -> usize {
    (low_nibble(a) as usize) << 8 | b as usize
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    fn parse(a: u8, b: u8) -> Self {
        match high_nibble(a) {
            0x0 => match b {
                0x00 => Instruction::Panic,
                0xe0 => Instruction::ClearDisplay,
                0xee => Instruction::Return,
                _ => panic!("Unimplemented."),
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
                    _ => panic!("Not implemented: F {:02x}", b),
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
                    _ => panic!("Not implemented: F {:02x}", b),
                }
            }
            _ => panic!("Not implemented: {:#02x} {:#02x}", a, b),
        }
    }

    fn to_bytes(self) -> (u8, u8) {
        match self {
            Instruction::StoreConst(x, value) => ((0x6 << 4) | x as u8, value),
            Instruction::Operator(x, y, operator) => {
                ((0x8 << 4) | x as u8, ((y as u8) << 4) | operator as u8)
            }
            _ => panic!("Not implemented."),
        }
    }
}

struct Vm {
    ram: [u8; 0x1000],
    v: [u8; 0x10],
    i: usize,
    pc: usize,
    stack: Stack,
    delay_timer: u8,
    sound_timer: u8,
    video: [[u8; 64]; 32],
}

impl Vm {
    fn new() -> Self {
        Vm {
            ram: [0; 0x1000],
            v: [0; 0x10],
            i: 0,
            pc: 0x200,
            stack: Stack::new(),
            delay_timer: 0,
            sound_timer: 0,
            video: [[0; 64]; 32],
        }
    }

    fn load(&mut self, program: Vec<u8>) {
        for i in 0..program.len() {
            self.ram[0x200 + i] = program[i]
        }
    }

    fn current_instruction(&self) -> (u8, u8) {
        (self.ram[self.pc], self.ram[self.pc + 1])
    }

    fn advance(&mut self) {
        self.pc += 2;
    }

    fn step(&mut self) {
        let (a, b) = self.current_instruction();
        self.advance();

        let instruction = Instruction::parse(a, b);

        println!("Instruction: {:?}", instruction);

        match instruction {
            Instruction::Panic => {
                for x in 0x0..0x10 {
                    println!("V{:#00x} {:#04x}", x, self.v[x]);
                }
                panic!("Hit empty instruction.");
            }
            Instruction::ClearDisplay => {
                self.video = [[0; 64]; 32];
            }
            Instruction::Return => {
                self.pc = self.stack.pop().unwrap();
            }
            Instruction::Jump(pc) => self.pc = pc,
            Instruction::Call(pc) => {
                self.stack.push(self.pc);
                self.pc = pc;
            }
            Instruction::SkipIfEqual(x, value) => {
                if self.v[x] == value {
                    self.advance();
                }
            }
            Instruction::SkipIfNotEqual(x, value) => {
                if self.v[x] != value {
                    self.advance();
                }
            }
            Instruction::SkipIfEqualV(x, y) => {
                if self.v[x] == self.v[y] {
                    self.advance();
                }
            }
            Instruction::SkipIfNotEqualV(x, y) => {
                if self.v[x] != self.v[y] {
                    self.advance();
                }
            }
            Instruction::StoreConst(x, value) => {
                self.v[x] = value;
            }
            Instruction::AddConst(x, value) => {
                let (result, _overflow) = self.v[x].overflowing_add(value);
                self.v[x] = result;
            }
            Instruction::Operator(x, y, op) => match op {
                Operator::Assign => self.v[x] = self.v[y],
                Operator::Or => self.v[x] |= self.v[y],
                Operator::And => self.v[x] &= self.v[y],
                Operator::Xor => self.v[x] ^= self.v[y],
                Operator::Add => {
                    let (result, overflow) = self.v[x].overflowing_add(self.v[y]);
                    self.v[0xf] = if overflow { 1 } else { 0 };
                    self.v[x] = result;
                }
                Operator::Sub => {
                    let (result, borrow) = self.v[x].overflowing_sub(self.v[y]);
                    self.v[0xf] = if !borrow { 1 } else { 0 };
                    self.v[x] = result;
                }
                Operator::LeftShift => {
                    self.v[0xf] = self.v[x] & 0x1;
                    self.v[x] >>= 1;
                }
                Operator::NegSub => {
                    let (result, borrow) = self.v[y].overflowing_sub(self.v[x]);
                    self.v[0xf] = if !borrow { 1 } else { 0 };
                    self.v[x] = result;
                }
                Operator::RightShift => {
                    self.v[0xf] = if self.v[x] & 0x80 != 0 { 1 } else { 0 };
                    self.v[x] <<= 1;
                }
            },
            Instruction::StoreI(addr) => {
                self.i = addr;
            }
            Instruction::JumpV0(addr) => self.pc = self.v[0] as usize + addr,
            Instruction::StoreRand(x, value) => {
                let r: u8 = random();
                self.v[x] = r & value
            }
            Instruction::Draw(x, y, height) => {
                let vy = self.v[y] as usize;
                let vx = self.v[x] as usize;

                println!("Draw: {}, {} ({})", vx, vy, height);

                for row in 0..(height as usize) {
                    let y = (vy + row) % 32;
                    for col in 0..8 {
                        let x = (vx + col) % 64;
                        let set = if self.ram[self.i + row] & (0x80 >> col) != 0 { 1 } else { 0 };
                        self.video[y][x] ^= set;
                    }
                }

                for row in 0..32 {
                    for col in 0..64 {
                        print!("{}", if self.video[row][col] != 0 { "X" } else {" "});
                    }
                    println!("");
                }
            }
            Instruction::SkipIfPressed(_x) => {}
            Instruction::SkipIfNotPressed(_x) => self.advance(),
            Instruction::GetDelay(x) => self.v[x] = self.delay_timer,
            Instruction::GetKey(x) => {
                println!("Get key: {}", x);
            }
            Instruction::SetDelay(x) => self.delay_timer = self.v[x],
            Instruction::SetSound(x) => self.sound_timer = self.v[x],
            Instruction::AddI(x) => self.i += self.v[x] as usize,
            Instruction::LoadSprite(x) => println!("Sprite addr: {}", x),
            Instruction::StoreBCD(x) => {
                let v = self.v[x];
                self.ram[self.i] = (v / 100) % 10;
                self.ram[self.i + 1] = (v / 10) % 10;
                self.ram[self.i + 2] = v % 10;
            }
            Instruction::DumpRegisters(x) => {
                for i in 0..x {
                    self.ram[self.i + i] = self.v[i]
                }
            }
            Instruction::LoadRegisters(x) => {
                for i in 0..x {
                    self.v[i] = self.ram[self.i + i]
                }
            }
        }

        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }
    }
}

pub fn assemble(program: Vec<Instruction>) -> Vec<u8> {
    let mut result = vec![];
    for ins in program {
        let (a, b) = ins.to_bytes();
        result.push(a);
        result.push(b);
    }
    result
}


fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("CHIP-8", 64 * 10, 32 * 10)
        .position_centered()
        .build()
        .unwrap();


    let mut canvas = window.into_canvas().build().unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut vm = Vm::new();

    let program = fs::read("pong.ch8").unwrap();

    // let program = assemble(vec!(
    //     Instruction::StoreConst(0, 0xff),
    //     Instruction::Operator(0, 0, Operator::RightShift),
    // ));

    vm.load(program);
    loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => {
                    return;
                }
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    return;
                }
                _ => {}
            }
        }

        vm.step();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}
