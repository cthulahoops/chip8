use std::fs;

use chip8lib::vm;

fn main() {
    let program = fs::read("Tetris.ch8").unwrap();

    vm::execute(program);
}
