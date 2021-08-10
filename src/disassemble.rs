use super::instructions::{Instruction, Operator};

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
