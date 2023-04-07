use std::collections::HashMap;

/**
 * TODO: Decide on trade-off:
 * Have them be vectors, or make each instruction a static 32 bits.
 **/
pub type Bytecode = Vec<u8>;

pub type Opcodes = [Option<Operation>; 256];

pub const OPCODES: Opcodes = {
    const INIT: Option<Operation> = None;
    let mut opcodes: Opcodes = [INIT; 256];
    let ops = [
        (Opcode::Const, "CONST", [2, 0]),
        (Opcode::Array, "ARRAY", [2, 0]),
        (Opcode::Len, "LEN", [0, 0]),
        (Opcode::Index, "INDEX", [0, 0]),
        (Opcode::SetIndex, "SET_INDEX", [0, 0]),
        (Opcode::Halt, "HALT", [0, 0]),
        (Opcode::True, "TRUE", [0, 0]),
        (Opcode::False, "FALSE", [0, 0]),
        (Opcode::Null, "NULL", [0, 0]),
        (Opcode::Add, "ADD", [0, 0]),
        (Opcode::Subtract, "SUB", [0, 0]),
        (Opcode::Multiply, "MUL", [0, 0]),
        (Opcode::Divide, "DIV", [0, 0]),
        (Opcode::Modulus, "MOD", [0, 0]),
        (Opcode::And, "AND", [0, 0]),
        (Opcode::Or, "OR", [0, 0]),
        (Opcode::Bang, "BANG", [0, 0]),
        (Opcode::Minus, "MINUS", [0, 0]),
        (Opcode::Equals, "EQ", [0, 0]),
        (Opcode::NotEquals, "NOT_EQ", [0, 0]),
        (Opcode::GreaterThan, "GT", [0, 0]),
        (Opcode::GreaterThanEquals, "GTE", [0, 0]),
        (Opcode::GetNative, "GETN", [1, 0]),
        (Opcode::SetGlobal, "SETG", [2, 0]),
        (Opcode::GetGlobal, "GETG", [2, 0]),
        (Opcode::Set, "SET", [1, 0]),
        (Opcode::Get, "GET", [1, 0]),
        (Opcode::Jump, "JMP", [2, 0]),
        (Opcode::JumpIfNot, "JMP_IF_NOT", [2, 0]),
        (Opcode::Pop, "POP", [0, 0]),
        (Opcode::Return, "RET", [0, 0]),
        (Opcode::Call, "CALL", [1, 0]),
        (Opcode::Closure, "CLOSURE", [2, 1]),
        (Opcode::SelfClosure, "SELF", [0, 0]),
        (Opcode::GetClosure, "GETC", [1, 0]),
        (Opcode::SetClosure, "SETC", [1, 0]),
        (Opcode::Note, "NOTE", [1, 0]),
        (Opcode::Control, "CC", [1, 0]),
        (Opcode::Rest, "REST", [1, 0]),
        (Opcode::Yield, "YIELD", [0, 0]),
        (Opcode::Next, "NEXT", [0, 0]),
    ];
    let mut i = 0;
    while i < ops.len() {
        let (opcode, name, operands) = ops[i];
        let mut num_operands = 0;
        if operands[1] != 0 {
            num_operands = 2;
        } else if operands[0] != 0 {
            num_operands = 1;
        }
        opcodes[opcode as usize] = Some(Operation {
            name,
            opcode,
            operands,
            num_operands,
            size: 1 + operands[0] + operands[1],
        });
        i += 1;
    }
    opcodes
};

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, FromPrimitive)]
pub enum Opcode {
  Const = 1,
  Array,
  Len,
  Index,
  SetIndex,

  True = 10,
  False,
  Null,

  Add = 20,
  Subtract,
  Multiply,
  Divide,
  Modulus,

  Bang = 30,
  Minus,

  And = 40,
  Or,

  Equals = 50,
  NotEquals,
  GreaterThan,
  GreaterThanEquals,

  GetNative = 60,

  SetGlobal = 70,
  GetGlobal,

  Set = 80,
  Get,

  Closure = 90,
  SelfClosure,
  GetClosure,
  SetClosure,

  Return = 100,
  Call,

  Jump = 110,
  JumpIfNot,

  Note = 200,
  Control,
  Rest,

  Yield = 210,
  Next,

  Pop = 253,
  NotImplemented = 254,
  Halt = 255,
}

pub struct Operation {
    name: &'static str,
    opcode: Opcode,
    operands: [u8; 2],
    num_operands: usize,
    size: u8,
}

/// Packs operand value of given bytes at an offset within an instruction array.
///
pub fn pack_big_endian(mut arr: &mut Bytecode, offset: u8, mut size: u8, value: i32) {
    let mut n = value;
    while size > 0 {
        size -= 1;
        arr[(offset + size) as usize] = (n & 255) as u8;
        n >>= 8;
    }
}

/// Retrieves operand value of the given bytes at an offset within an
/// instruction array.
///
pub fn unpack_big_endian(arr: &mut Bytecode, offset: u32, size: u32) -> u32 {
    let mut n = 0;
    let base: u32 = 256;
    for i in 0..size {
        n += (arr[(offset + i) as usize] as u32) * base.pow(size - i - 1);
    }
    n
}

/// Create new instruction, packing operands in big-endian byte order.
///
pub fn create_instruction(opcode: Opcode, op1: i32, op2: i32) -> Bytecode {
    let operation = &OPCODES[opcode as usize];
    if operation.is_none() {
        return Vec::new();
    }
    let operation = operation.as_ref().unwrap();

    let mut instruction = Vec::new();
    instruction.push(opcode as u8);

    if operation.size <= 1 {
        return instruction;
    }

    let mut offset: u8 = 1;
    pack_big_endian(&mut instruction, offset, operation.operands[0], op1);
    offset += operation.operands[0];

    if operation.num_operands > 1 {
        pack_big_endian(&mut instruction, offset, operation.operands[1], op2);
    }

    instruction
}

/// Disassemble a bytecode into a more human-readable format.
///
pub fn disassemble(opcodes: &Opcodes, mut bytecode: &mut Bytecode) -> Option<String> {
    let mut pos = 0;
    let mut output = String::from("");

    while pos < bytecode.len() {
        let byte = bytecode[pos];
        let opcode: Option<Opcode> = num::FromPrimitive::from_u8(byte);
        let operation = &opcodes[opcode? as usize];
        let operation = operation.as_ref().unwrap();
        let address = format!("{:0>4}", pos);

        pos += 1;
        if operation.num_operands == 0 {
            output = format!("{}{} {}\n", output, address, &operation.name);
            continue;
        }

        let mut args: Vec<String> = Vec::new();
        for operand in operation.operands {
            args.push(format!("{}", unpack_big_endian(bytecode, pos as u32, operand as u32)));
            pos += operand as usize;
        }
        output = format!("{}{} {} ({})\n", output, address, &operation.name, args.join(", "));
    }

    Some(output)
}
