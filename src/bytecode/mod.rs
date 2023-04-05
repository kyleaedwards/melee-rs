use std::collections::HashMap;

/**
 * TODO: Decide on trade-off:
 * Have them be vectors, or make each instruction a static 32 bits.
 **/
pub type Bytecode = Vec<u8>;

pub type Opcodes = HashMap<Opcode, Operation>;

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
    name: String,
    opcode: Opcode,
    operands: [u8; 2],
    num_operands: usize,
    size: u8,
}

pub fn get_opcodes() -> HashMap<Opcode, Operation> {
    let mut opcodes = HashMap::new();
    for (opcode, name, operands) in [
        (Opcode::Const, String::from("CONST"), [2, 0]),
        (Opcode::Array, String::from("ARRAY"), [2, 0]),
        (Opcode::Len, String::from("LEN"), [0, 0]),
        (Opcode::Index, String::from("INDEX"), [0, 0]),
        (Opcode::SetIndex, String::from("SET_INDEX"), [0, 0]),
        (Opcode::Halt, String::from("HALT"), [0, 0]),
        (Opcode::True, String::from("TRUE"), [0, 0]),
        (Opcode::False, String::from("FALSE"), [0, 0]),
        (Opcode::Null, String::from("NULL"), [0, 0]),
        (Opcode::Add, String::from("ADD"), [0, 0]),
        (Opcode::Subtract, String::from("SUB"), [0, 0]),
        (Opcode::Multiply, String::from("MUL"), [0, 0]),
        (Opcode::Divide, String::from("DIV"), [0, 0]),
        (Opcode::Modulus, String::from("MOD"), [0, 0]),
        (Opcode::And, String::from("AND"), [0, 0]),
        (Opcode::Or, String::from("OR"), [0, 0]),
        (Opcode::Bang, String::from("BANG"), [0, 0]),
        (Opcode::Minus, String::from("MINUS"), [0, 0]),
        (Opcode::Equals, String::from("EQ"), [0, 0]),
        (Opcode::NotEquals, String::from("NOT_EQ"), [0, 0]),
        (Opcode::GreaterThan, String::from("GT"), [0, 0]),
        (Opcode::GreaterThanEquals, String::from("GTE"), [0, 0]),
        (Opcode::GetNative, String::from("GETN"), [1, 0]),
        (Opcode::SetGlobal, String::from("SETG"), [2, 0]),
        (Opcode::GetGlobal, String::from("GETG"), [2, 0]),
        (Opcode::Set, String::from("SET"), [1, 0]),
        (Opcode::Get, String::from("GET"), [1, 0]),
        (Opcode::Jump, String::from("JMP"), [2, 0]),
        (Opcode::JumpIfNot, String::from("JMP_IF_NOT"), [2, 0]),
        (Opcode::Pop, String::from("POP"), [0, 0]),
        (Opcode::Return, String::from("RET"), [0, 0]),
        (Opcode::Call, String::from("CALL"), [1, 0]),
        (Opcode::Closure, String::from("CLOSURE"), [2, 1]),
        (Opcode::SelfClosure, String::from("SELF"), [0, 0]),
        (Opcode::GetClosure, String::from("GETC"), [1, 0]),
        (Opcode::SetClosure, String::from("SETC"), [1, 0]),
        (Opcode::Note, String::from("NOTE"), [1, 0]),
        (Opcode::Control, String::from("CC"), [1, 0]),
        (Opcode::Rest, String::from("REST"), [1, 0]),
        (Opcode::Yield, String::from("YIELD"), [0, 0]),
        (Opcode::Next, String::from("NEXT"), [0, 0]),
    ] {
        let mut num_operands = 0;
        if operands[1] != 0 {
            num_operands = 2;
        } else if operands[0] != 0 {
            num_operands = 1;
        }
        opcodes.insert(
            opcode,
            Operation {
                name,
                opcode,
                operands,
                num_operands,
                size: 1 + operands[0] + operands[1],
            }
        );
    }
    opcodes
}

pub fn createInstruction(opcode: Opcode, ) {

}

/// Packs operand value of given bytes at an offset within an instruction array.
///
pub fn pack_big_endian(mut arr: &mut Bytecode, offset: u8, mut size: u8, value: u8) {
    let mut n = value;
    while size > 0 {
        size -= 1;
        arr[(offset + size) as usize] = n & 255;
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
pub fn create_instruction(opcodes: &Opcodes, opcode: Opcode, args: Vec<u8>) -> Bytecode {
    let operation = opcodes.get(&opcode);
    if operation.is_none() {
        return Vec::new();
    }
    let operation = operation.unwrap();

    let mut instruction = Vec::new();
    instruction.push(opcode as u8);

    if operation.size <= 1 {
        return instruction;
    }

    let mut offset = 1;
    for i in 0..operation.num_operands {
        pack_big_endian(&mut instruction, offset, operation.operands[i], args[i]);
        offset += operation.operands[i];
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
        let operation = opcodes.get(opcode.as_ref().unwrap())?;
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
