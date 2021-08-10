use rand::prelude::*;
use std::fs;

use std::time::Duration;

use std::io::{stdout, Write};
use termion::color;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

const FONT: [u8; 16 * 5] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

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

fn bcd(v: u8) -> [u8; 3] {
    [v / 100 % 10, v / 10 % 10, v % 10]
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
    fn parse(a: u8, b: u8) -> Result<Self, String> {
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
    redraw: bool,
    key: Option<u8>,
}

impl Vm {
    fn new() -> Self {
        let mut ram = [0; 0x1000];
        ram[0..16 * 5].copy_from_slice(&FONT);

        Vm {
            ram: ram,
            v: [0; 0x10],
            i: 0,
            pc: 0x200,
            stack: Stack::new(),
            delay_timer: 0,
            sound_timer: 0,
            video: [[0; 64]; 32],
            redraw: false,
            key: None,
        }
    }

    fn load(&mut self, program: Vec<u8>) {
        self.ram[0x200..0x200 + program.len()].copy_from_slice(&program);
    }

    fn current_instruction(&self) -> Instruction {
        Instruction::parse(self.ram[self.pc], self.ram[self.pc + 1]).unwrap()
    }

    fn advance(&mut self) {
        self.pc += 2;
    }

    fn step(&mut self) {
        let instruction = self.current_instruction();
        self.advance();

        // println!("Instruction: {:?}", instruction);

        match instruction {
            Instruction::Panic => {
                for x in 0x0..0x10 {
                    println!("V{:#00x} {:#04x}", x, self.v[x]);
                }
                panic!("Hit empty instruction.");
            }
            Instruction::ClearDisplay => {
                self.video = [[0; 64]; 32];
                self.redraw = true;
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

                let mut collision = 0;

                for row in 0..(height as usize) {
                    let y = (vy + row) % 32;
                    for col in 0..8 {
                        let x = (vx + col) % 64;
                        let set = if self.ram[self.i + row] & (0x80 >> col) != 0 {
                            1
                        } else {
                            0
                        };
                        collision |= self.video[y][x] & set;
                        self.video[y][x] ^= set;
                    }
                }

                self.v[0xf] = collision;
                self.redraw = true;
            }

            Instruction::SkipIfPressed(x) => {
                let expected_key = self.v[x];
                if Some(expected_key) == self.key {
                    self.advance();
                    self.key = None;
                }
            }
            Instruction::SkipIfNotPressed(x) => {
                let expected_key = self.v[x];
                if Some(expected_key) != self.key {
                    self.advance();
                } else {
                    self.key = None;
                }
            }
            Instruction::GetDelay(x) => self.v[x] = self.delay_timer,
            Instruction::GetKey(x) => {
                panic!("Get key: {}", x);
            }
            Instruction::SetDelay(x) => self.delay_timer = self.v[x],
            Instruction::SetSound(x) => self.sound_timer = self.v[x],
            Instruction::AddI(x) => self.i += self.v[x] as usize,
            Instruction::LoadSprite(x) => self.i = (self.v[x] as usize) * 5,
            Instruction::StoreBCD(x) => {
                self.ram[self.i..self.i + 3].copy_from_slice(&bcd(self.v[x]));
            }
            Instruction::DumpRegisters(x) => {
                for i in 0..(x + 1) {
                    self.ram[self.i + i] = self.v[i]
                }
            }
            Instruction::LoadRegisters(x) => {
                for i in 0..(x + 1) {
                    self.v[i] = self.ram[self.i + i]
                }
            }
        }
    }

    fn tick(&mut self) {
        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }

        if self.sound_timer > 0 {
            self.sound_timer -= 1;
        }
    }

    fn set_key(&mut self, key: u8) {
        self.key = Some(key);
    }
}

fn render(video: [[u8; 64]; 32]) {
    println!("{}", termion::cursor::Goto(1, 1));
    for row in 0..32 {
        for col in 0..64 {
            if video[row][col] != 0 {
                print!("{}O{}", color::Bg(color::Green), color::Bg(color::Reset));
            } else {
                print!(" ");
            }
        }
        print!("\n\r");
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

fn event_loop(mut vm: Vm) {
    let mut clock = 0;

    // Get the standard input stream.
    let stdin = termion::async_stdin();
    let mut events = stdin.events();

    print!(
        "{}{}{}",
        termion::clear::All,
        termion::cursor::Goto(1, 1),
        termion::cursor::Hide
    );
    loop {
        vm.step();
        clock += 1;
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 600));

        match events.next() {
            Some(event) => {
                let event = event.unwrap();

                match event {
                    Event::Key(Key::Ctrl('c')) | Event::Key(Key::Esc) => {
                        return;
                    }
                    Event::Key(Key::Char(c)) => {
                        if let Some(key) = match c {
                            '1' => Some(0x00),
                            '2' => Some(0x01),
                            '3' => Some(0x02),
                            '4' => Some(0x03),
                            '\'' => Some(0x04),
                            ',' => Some(0x05),
                            '.' => Some(0x06),
                            'p' => Some(0x07),
                            'a' => Some(0x08),
                            'o' => Some(0x09),
                            'e' => Some(0x0a),
                            'u' => Some(0x0b),
                            ';' => Some(0x0c),
                            'q' => Some(0x0d),
                            'j' => Some(0x0e),
                            'k' => Some(0x0f),
                            _ => None,
                        } {
                            vm.set_key(key);
                        }
                    }
                    _ => print!("{:?}\n\r", event),
                }
            }
            None => {}
        }

        if clock % 10 == 0 {
            vm.tick();

            if vm.redraw {
                render(vm.video);
                vm.redraw = false
            }
        }
    }
}

pub fn execute(program: Vec<u8>) {
    let mut vm = Vm::new();
    vm.load(program);

    let mut stdout = stdout().into_raw_mode().unwrap();
    event_loop(vm);
    write!(stdout, "{}", termion::cursor::Show).unwrap();
}

fn reffed_data(instruction: &Result<Instruction, String>) -> Option<usize> {
    match instruction {
        Ok(Instruction::StoreI(value)) => Some(*value),
        _ => None,
    }
}

fn reffed_label(instruction: &Result<Instruction, String>) -> Option<usize> {
    match instruction {
        Ok(Instruction::StoreI(value)) => Some(*value),
        Ok(Instruction::Call(value)) => Some(*value),
        Ok(Instruction::Jump(value)) => Some(*value),
        _ => None,
    }
}

fn format_op(operator: Operator) -> String {
    match operator {
        Operator::Assign => format!(":="),
        Operator::Or => format!("|="),
        Operator::And => format!("&="),
        Operator::Xor => format!("^="),
        Operator::Add => format!("+="),
        Operator::Sub => format!("-="),
        Operator::RightShift => format!(">>="),
        Operator::NegSub => format!("<-=>"),
        Operator::LeftShift => format!("<<="),
    }
}

fn format_ins(instruction: Instruction) -> String {
    match instruction {
        Instruction::StoreConst(x, value) => format!("v{:x} := {}\n", x, value),
        Instruction::AddConst(x, value) => format!("v{:x} += {}\n", x, value),
        Instruction::StoreI(value) => format!("i := {}\n", value),
        Instruction::AddI(x) => format!("i += {:x}\n", x),
        Instruction::Draw(x, y, value) => format!("draw(v{:x}, v{:x}, {})\n", x, y, value),
        Instruction::SkipIfEqual(x, value) => format!("if v{:x} != {}:", x, value),
        Instruction::SkipIfNotEqual(x, value) => format!("if v{:x} == {}:", x, value),
        Instruction::SkipIfNotEqualV(x, y) => format!("if v{:x} == v{:x}:", x, y),
        Instruction::SkipIfNotPressed(x) => format!("if key(v{:x}):", x),
        Instruction::SkipIfPressed(x) => format!("if !key(v{:x}):", x),
        Instruction::StoreRand(x, value) => format!("v{:x} := random({:#x})\n", x, value),
        Instruction::Operator(x, y, op) => format!("v{:x} {} v{:x}\n", x, format_op(op), y),
        Instruction::StoreBCD(x) => format!("store_bcd(v{:x})\n", x),
        _ => format!("{:?}\n", instruction),
    }
}

pub fn disassemble(program: Vec<u8>) {
    let mut instructions = vec![];

    for i in 0..program.len() {
        if i % 2 == 1 {
            continue;
        }

        instructions.push(Instruction::parse(program[i], program[i + 1]));
    }

    let labels: std::collections::HashSet<usize> =
        instructions.iter().filter_map(reffed_label).collect();
    let data: std::collections::HashSet<usize> =
        instructions.iter().filter_map(reffed_data).collect();

    let mut c: usize = 0x200;
    let mut data_format = false;

    for instruction in instructions {
        if labels.contains(&c) {
            println!("{}:", c);
            if data.contains(&c) {
                data_format = true;
            } else {
                data_format = false;
            }
        }

        if data_format {
            println!("{:08b}\n{:08b}", program[c - 0x200], program[c - 0x1ff])
        } else {
            match instruction {
                Ok(instruction) => print!("   {}", format_ins(instruction)),
                Err(s) => println!("   ERR: {}", s),
            }
        }

        c += 2;
    }
}

fn main() {
    let program = fs::read("Tetris.ch8").unwrap();

    execute(program);

    return;
}
