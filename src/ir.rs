use crate::arena::*;
use crate::colours::*;
use crate::compile::CompilationError;
use crate::lexer::*;
use crate::parser::*;
use crate::typechecker::*;
use core::fmt;
use core::panic;
use std::collections::HashMap;
use std::fmt::Display;
use std::println;

/*

We start by converting the AST to a basic CFG (control flow graph).
The CFG should be similar to an IR, but not SSA initially, as
we would like to perform a dominator algorithm to figure out
where to put phi instructions.

AST -> 3AC blocks (not SSA). Type check while doing this? -> Compute Dominator Frontiers -> SSA


// Allocate memory on stack frame of current function
// Push down the stack frame, essentially
alloca [Type?], [alignment?]

// Load and store
store %reg, %addr, alignment
%reg = load %addr, alignment

ret %reg
*/

// The types for the IR are much simpler than the C types and map
// much more easily to machine instruction types

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum IRType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    F128,

    // While the details are not quite ironed out, the goal is for
    // pointers to be opaque type-wise. Load or store instructions
    // need to specify the data they wish to operate on with a ptr.
    Ptr,
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub struct Register<'v> {
    disp_name: Option<&'v str>,

    uuid: u32,
    version: Option<u32>,
    ir_type: IRType,
}

impl<'v> Display for Register<'v> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.disp_name {
            Some(name) => write!(f, "{B_BLUE_START}%{}.{}{FORMAT_END}", name, self.uuid),
            None => write!(f, "{B_BLUE_START}%None.{}{FORMAT_END}", self.uuid),
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Constant {
    I1(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    //F128(f128),
}

fn int_to_constant(val: i64, ir_type: IRType) -> Constant {
    use Constant::*;
    match ir_type {
        IRType::I1 => I1(val != 0),
        IRType::I8 => I8(val as i8),
        IRType::I16 => I16(val as i16),
        IRType::I32 => I32(val as i32),
        IRType::I64 | IRType::Ptr => I64(val as i64),
        IRType::I128 => I128(val as i128),
        IRType::F32 => F32(val as f32),
        IRType::F64 => F64(val as f64),
        _ => panic!("Int to constant for {ir_type:?} not implemented."),
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Operand<'o> {
    Register(Register<'o>),
    Constant(Constant),
}

impl<'v> Display for Operand<'v> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(reg) => write!(f, "{}", reg),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ArithmeticFlags {
    Nsw, // No signed wrap
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum ComparisonKind {
    Eq,
}

type BlockID = usize;

fn get_next_block<'a>(blocks: &mut Vec<BasicBlockData<'a>>) -> BlockID {
    blocks.push(new_bb());
    return blocks.len() - 1;
}

// TODO: Consider removing IRType for some of these operations
// where the dst variable contains the relevant IRType.
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Instruction<'i> {
    Alloca {
        dst_type: IRType,
        dst: Register<'i>,
    },
    Load {
        dst_type: IRType,
        dst: Register<'i>,
        src_type: IRType,
        src: Register<'i>,
    },
    Store {
        src_type: IRType,
        src: Operand<'i>,
        dst_type: IRType,
        dst: Register<'i>,
    },
    Add {
        dst: Register<'i>,
        ir_type: IRType,
        flags: Option<ArithmeticFlags>,
        left: Operand<'i>,
        right: Operand<'i>,
    },
    Mul {
        dst: Register<'i>,
        ir_type: IRType,
        flags: Option<ArithmeticFlags>,
        left: Operand<'i>,
        right: Operand<'i>,
    },
    ZeroExtend {
        dst: Register<'i>,
        src: Operand<'i>,
        src_type: IRType,
        dst_type: IRType,
    },
    SignExtend {
        dst: Register<'i>,
        src: Operand<'i>,
        src_type: IRType,
        dst_type: IRType,
    },
    Truncate {
        dst: Register<'i>,
        src: Operand<'i>,
        src_type: IRType,
        dst_type: IRType,
    },
    _SiToFp {
        dst: Register<'i>,
        src: Operand<'i>,
        src_type: IRType,
        dst_type: IRType,
    },
    Assign {
        dst: Register<'i>,
        src: Operand<'i>,
    },
    Icmp {
        dst: Register<'i>,
        comparison: ComparisonKind,
        op_type: IRType,
        op1: Operand<'i>,
        op2: Operand<'i>,
    },
    _FunctionCall {
        dst: Register<'i>,
        parameters: &'i [&'i Register<'i>],
    },
}

// Instructions that terminate a basic block
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Terminator<'t> {
    Brcond {
        condition: Operand<'t>,
        if_true_dst: BlockID,
        if_false_dst: BlockID,
    },
    Br {
        dst: BlockID,
    },
}

impl<'i> Display for Instruction<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{RED_START}")?;
        match self {
            Instruction::Alloca { .. } => write!(f, "alloca")?,
            Instruction::Load { .. } => write!(f, "load")?,
            Instruction::Store { .. } => write!(f, "store")?,
            Instruction::Add { .. } => write!(f, "add")?,
            Instruction::Mul { .. } => write!(f, "mul")?,
            Instruction::SignExtend { .. } => write!(f, "sext")?,
            Instruction::ZeroExtend { .. } => write!(f, "zext")?,
            Instruction::Icmp { .. } => write!(f, "icmp")?,
            _ => return write!(f, "{FORMAT_END}Display unimpl. for {self:?}"),
        }
        write!(f, "{FORMAT_END}")
    }
}

impl<'i> Display for Terminator<'i> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{RED_START}")?;
        match self {
            Terminator::Brcond { .. } => write!(f, "br")?,
            Terminator::Br { .. } => write!(f, "br")?,
            _ => return write!(f, "{FORMAT_END}Display unimpl. for {self:?}"),
        }
        write!(f, "{FORMAT_END}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlockData<'b> {
    instructions: Vec<Instruction<'b>>,

    terminator: Option<Terminator<'b>>,
    // successors: Vec<BlockID>,
}

fn new_bb<'b>() -> BasicBlockData<'b> {
    BasicBlockData {
        instructions: Vec::<Instruction>::new(),
        // successors: Vec::<BlockID>::new(),
        terminator: None,
    }
}

pub struct IRState<'s, 'a> {
    uuid_count: u32,
    scopes: &'s mut Scopes<'a, 'a>,
    blocks: &'s mut Vec<BasicBlockData<'a>>,
    arena: &'a Arena,
    // blocks: Vec<&'s BasicBlock<'a>>,
}

fn print_basic_block(bb: &BasicBlockData) {
    for instruction in &bb.instructions {
        match instruction {
            Instruction::Alloca { dst_type, dst } => {
                println!("{dst} = {RED_START}alloca{FORMAT_END} {dst_type:?}")
            }
            Instruction::Store {
                src_type,
                src,
                dst_type,
                dst,
            } => match src {
                Operand::Register(reg) => {
                    println!("{RED_START}store{FORMAT_END} {src_type:?} {reg}, {dst_type:?} {dst}")
                }
                Operand::Constant(c) => {
                    println!("{RED_START}store{FORMAT_END} {src_type:?} {c:?}, ptr {dst}")
                }
            },
            Instruction::Load {
                dst_type,
                dst,
                src_type,
                src,
            } => println!("{dst} = {instruction} {dst_type:?}, {src_type:?} {src}"),
            Instruction::Add {
                dst,
                ir_type,
                flags,
                left,
                right,
            }
            | Instruction::Mul {
                dst,
                ir_type,
                flags,
                left,
                right,
            } => println!("{dst} = {instruction} {flags:?} {ir_type:?}, {left} {right}"),
            Instruction::Assign { dst, src } => println!("{dst} = {src}"),
            Instruction::SignExtend {
                dst,
                src,
                src_type,
                dst_type,
            }
            | Instruction::ZeroExtend {
                dst,
                src,
                src_type,
                dst_type,
            } => {
                println!("{dst} = {instruction} {src} {src_type:?} to {dst_type:?}")
            }
            Instruction::Truncate {
                dst,
                src,
                src_type,
                dst_type,
            } => {
                println!("{dst} = {RED_START}trunc{FORMAT_END} {src_type:?} {src} to {dst_type:?}")
            }
            Instruction::Icmp {
                dst,
                comparison,
                op_type,
                op1,
                op2,
            } => println!("{dst} = {instruction} {comparison:?} {op_type:?} {op1} {op2}"),
            _ => println!("Print not implemented for {:?}", instruction),
        }
    }
    if let Some(terminator) = bb.terminator {
        match terminator {
            Terminator::Br { dst } => println!("{terminator} bb {dst}"),
            Terminator::Brcond {
                condition,
                if_true_dst,
                if_false_dst,
            } => println!("{terminator} i1 {condition}, bb {if_true_dst}, bb {if_false_dst}"),
        }
    }
}

fn specifier_to_ir_type(type_specifier: &TypeSpecifier) -> IRType {
    match type_specifier {
        TypeSpecifier::SignedChar => IRType::I8,
        TypeSpecifier::UnsignedChar => IRType::I8,
        TypeSpecifier::Short => IRType::I16,
        TypeSpecifier::UnsignedShort => IRType::I16,
        TypeSpecifier::Int => IRType::I32,
        TypeSpecifier::Long => IRType::I64,
        TypeSpecifier::UnsignedInt => IRType::I32,
        TypeSpecifier::UnsignedLong => IRType::I64,
        TypeSpecifier::LongLong => IRType::I128,
        TypeSpecifier::UnsignedLongLong => IRType::I128,

        TypeSpecifier::Float => IRType::F32,
        TypeSpecifier::Double => IRType::F64,
        TypeSpecifier::LongDouble => IRType::F128,

        _ => panic!("Not implemented this type specifier to IR conversion."),
    }
}

fn next_uuid<'s, 'a>(ir_state: &mut IRState) -> u32 {
    ir_state.uuid_count += 1;
    return ir_state.uuid_count;
}

fn new_temp_reg<'s, 'a: 's>(ir_state: &'s mut IRState, ir_type: IRType) -> Register<'a> {
    Register {
        disp_name: None,
        uuid: next_uuid(ir_state),
        version: None,
        ir_type,
    }
}

enum CastOp {
    SignExtend,
    Trunc,
    ZeroExtend,
}

fn cast_operation<'s, 'i, 'a>(
    from_info: VariableInfo<'a>,
    to_type: Type<'a>,
    cast_op: CastOp,
    ir_state: &'i mut IRState<'s, 'a>,
    cur_bb: BlockID,
) -> Result<VariableInfo<'a>, CompilationError<'a>> {
    // let (src_type_spec, dst_type_spec, src_reg) = match direction {
    //     PromotionDirection::LeftToRight => (l_type_spec, r_type_spec, left_reg),
    //     PromotionDirection::RightToLeft => {
    //         (r_type_spec, l_type_spec, right_reg)
    //     }
    // };
    let to_ir_type = specifier_to_ir_type(&to_type.specifiers.type_specifier);
    let from_ir_type = specifier_to_ir_type(&from_info.c_type.specifiers.type_specifier);
    println!("Casting from {from_ir_type:?} to {to_type:?}");
    let dst = new_temp_reg(ir_state, to_ir_type);

    let cast_instr = match cast_op {
        CastOp::ZeroExtend => Instruction::ZeroExtend {
            dst,
            src: Operand::Register(from_info.reg),
            src_type: from_ir_type,
            dst_type: to_ir_type,
        },
        CastOp::SignExtend => Instruction::SignExtend {
            dst,
            src: Operand::Register(from_info.reg),
            src_type: from_ir_type,
            dst_type: to_ir_type,
        },
        CastOp::Trunc => Instruction::Truncate {
            dst,
            src: Operand::Register(from_info.reg),
            src_type: from_ir_type,
            dst_type: to_ir_type,
        },
    };
    ir_state.blocks[cur_bb].instructions.push(cast_instr);

    // match direction {
    //     PromotionDirection::LeftToRight => left_reg = dst,
    //     PromotionDirection::RightToLeft => right_reg = dst,
    // }
    Ok(VariableInfo {
        c_type: to_type,
        is_lvalue: false,
        reg: dst,
    })
    // Ok((specifier_to_type(from_type)?, from_type, dst))
}

// const INT_SPECIFIER = TypeSpecifier::Int;i
const INT_SPECIFIERS: Specifiers = Specifiers {
    type_specifier: TypeSpecifier::Int,
    is_const: false,
    is_volatile: false,
    is_typedef: false,
    is_extern: false,
    is_static: false,
};
const INT_TYPE: Type = Type {
    derived_types: &[],
    specifiers: &INT_SPECIFIERS,
};

fn integral_promotion<'i, 's, 'a>(
    var_info: &VariableInfo<'a>,
    ir_state: &'i mut IRState<'s, 'a>,
    cur_bb: BlockID,
) -> Result<VariableInfo<'a>, CompilationError<'a>> {
    match var_info.c_type.specifiers.type_specifier {
        TypeSpecifier::SignedChar
        | TypeSpecifier::UnsignedChar
        | TypeSpecifier::Short
        | TypeSpecifier::UnsignedShort => {
            cast_operation(*var_info, INT_TYPE, CastOp::SignExtend, ir_state, cur_bb)
        }
        _ => Ok(*var_info),
    }
}

fn cast<'s, 'i, 'r, 'a>(
    left_info: &'r mut VariableInfo<'a>,
    right_info: &'r mut VariableInfo<'a>,
    operator: TokenType,
    ir_state: &'i mut IRState<'s, 'a>,
    cur_bb: BlockID,
) -> Result<(), CompilationError<'a>> {
    if left_info.c_type.derived_types.is_empty() && right_info.c_type.derived_types.is_empty() {
        // Both left and right are arithmetic, perform type promotion

        let l_type_spec = left_info.c_type.specifiers.type_specifier;
        let r_type_spec = right_info.c_type.specifiers.type_specifier;

        let is_assigment = match operator {
            TokenType::Assign
            | TokenType::PlusAssign
            | TokenType::MinusAssign
            | TokenType::MultAssign
            | TokenType::DivAssign
            | TokenType::ModAssign
            | TokenType::LeftShiftAssign
            | TokenType::RightShiftAssign
            | TokenType::ANDAssign
            | TokenType::ORAssign
            | TokenType::XORAssign => true,
            _ => false,
        };

        if is_assigment && left_info.is_lvalue == false {
            return Err(CompilationError::AssigningToRValue);
        }

        println!("Casting... {l_type_spec:?} {r_type_spec:?}");

        // Usual Arithmetic Conversions from https://wiki.sei.cmu.edu/confluence/display/c/INT02-C.+Understand+integer+conversion+rules
        // which paraphrases the C89 standard http://port70.net/%7Ensz/c/c89/c89-draft.html#3.2.1.5

        // The usual arithmetic conversions are rules that provide a mechanism to yield a common type when both operands
        // of a binary operator are balanced to a common type or the second and third operands of the
        // conditional operator ( ? : ) are balanced to a common type.
        // Conversions involve two operands of different types, and one or both operands may be converted.
        // Many operators that accept arithmetic operands perform conversions using the usual arithmetic conversions.
        // After integer promotions are performed on both operands, the following rules are applied to the promoted operands:

        //     1. If both operands have the same type, no further conversion is needed.
        //     2. If both operands are of the same integer type (signed or unsigned), the operand with
        //        the type of lesser integer conversion rank is converted to the type of the operand with greater rank.
        //     3. If the operand that has unsigned integer type has rank greater than or equal to the rank of the type
        //        of the other operand, the operand with signed integer type is converted to the type of the operand
        //        with unsigned integer type.
        //     4. If the type of the operand with signed integer type can represent all of the values of the type of the
        //        operand with unsigned integer type, the operand with unsigned integer type is converted to the type
        //        of the operand with signed integer type.
        //     5. Otherwise, both operands are converted to the unsigned integer type corresponding to the type of the
        //        operand with signed integer type.

        // Integral promotions
        *right_info = integral_promotion(&right_info, ir_state, cur_bb)?;

        if let TokenType::Assign = operator {
            ()
        } else {
            *left_info = integral_promotion(&left_info, ir_state, cur_bb)?;
        }

        if l_type_spec == r_type_spec {
            return Ok(());
        }

        // TODO: This currently only works for integers.
        // TODO: Extend to work with FP and chars.
        // TODO: Truncation cases could maybe be combined.
        match (is_assigment, l_type_spec, r_type_spec) {
            (_, TypeSpecifier::LongDouble, _) => {
                panic!("Not implemented");
                // *right_info =
                //     cast_operation(*right_info, *left_info, CastOp::SignExtend, ir_state, bb)?
            }
            (_, _, TypeSpecifier::LongDouble) => {
                panic!("Not implemented");
                // *left_info =
                //     cast_operation(*left_info, *right_info, CastOp::SignExtend, ir_state, bb)?
            }

            (_, TypeSpecifier::Double, _) => {
                panic!("Not implemented");
                // *right_info =
                //     cast_operation(*right_info, *left_info, CastOp::SignExtend, ir_state, bb)?
            }
            (_, _, TypeSpecifier::Double) => {
                panic!("Not implemented");
                // *left_info =
                //     cast_operation(*left_info, *right_info, CastOp::SignExtend, ir_state, bb)?
            }

            (_, TypeSpecifier::Float, _) => {
                panic!("Not implemented");
                // *right_info =
                //     cast_operation(*right_info, *left_info, CastOp::SignExtend, ir_state, bb)?
            }
            (_, _, TypeSpecifier::Float) => {
                panic!("Not implemented");
                // *left_info =
                //     cast_operation(*left_info, *right_info, CastOp::SignExtend, ir_state, bb)?;
            }

            // UNSIGNED INTEGER PROMOTIONS
            (
                _,
                TypeSpecifier::UnsignedLongLong,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedInt,
            )
            | (_, TypeSpecifier::UnsignedLong, TypeSpecifier::UnsignedInt) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    cur_bb,
                )?
            }

            (
                false,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedInt,
                TypeSpecifier::UnsignedLongLong,
            )
            | (false, TypeSpecifier::UnsignedInt, TypeSpecifier::UnsignedLong) => {
                *left_info = cast_operation(
                    *left_info,
                    right_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    cur_bb,
                )?
            }

            (
                true,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedInt,
                TypeSpecifier::UnsignedLongLong,
            )
            | (true, TypeSpecifier::UnsignedInt, TypeSpecifier::UnsignedLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::Trunc,
                    ir_state,
                    cur_bb,
                )?
            }

            // SIGNED INTEGER PROMOTIONS
            (_, TypeSpecifier::LongLong, TypeSpecifier::Long | TypeSpecifier::Int)
            | (_, TypeSpecifier::Long, TypeSpecifier::Int) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::SignExtend,
                    ir_state,
                    cur_bb,
                )?
            }

            (false, TypeSpecifier::Long | TypeSpecifier::Int, TypeSpecifier::LongLong)
            | (false, TypeSpecifier::Int, TypeSpecifier::Long) => {
                *left_info = cast_operation(
                    *left_info,
                    right_info.c_type,
                    CastOp::SignExtend,
                    ir_state,
                    cur_bb,
                )?
            }

            (true, TypeSpecifier::Long | TypeSpecifier::Int, TypeSpecifier::LongLong)
            | (true, TypeSpecifier::Int, TypeSpecifier::Long) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::Trunc,
                    ir_state,
                    cur_bb,
                )?
            }

            // CONVERSIONS FROM SIGNED TO UNSIGNED
            // Cases where the unsigned operand has a greater or equal rank ->
            // converted the signed operand to unsigned.

            // When equal rank, just interpret the signed operand as unsigned
            (_, TypeSpecifier::UnsignedInt, TypeSpecifier::Int)
            | (_, TypeSpecifier::UnsignedLong, TypeSpecifier::Long)
            | (_, TypeSpecifier::UnsignedLongLong, TypeSpecifier::LongLong) => {
                *right_info = VariableInfo {
                    c_type: left_info.c_type,
                    is_lvalue: right_info.is_lvalue,
                    reg: right_info.reg,
                };
            }

            (false, TypeSpecifier::Int, TypeSpecifier::UnsignedInt)
            | (false, TypeSpecifier::Long, TypeSpecifier::UnsignedLong)
            | (false, TypeSpecifier::LongLong, TypeSpecifier::UnsignedLongLong) => {
                *left_info = VariableInfo {
                    c_type: right_info.c_type,
                    is_lvalue: left_info.is_lvalue,
                    reg: left_info.reg,
                };
            }

            (true, TypeSpecifier::Int, TypeSpecifier::UnsignedInt)
            | (true, TypeSpecifier::Long, TypeSpecifier::UnsignedLong)
            | (true, TypeSpecifier::LongLong, TypeSpecifier::UnsignedLongLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::Trunc,
                    ir_state,
                    cur_bb,
                )?
            }

            // When greater rank, zero extend the signed operand
            (
                _,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong,
                TypeSpecifier::Int,
            )
            | (_, TypeSpecifier::UnsignedLongLong, TypeSpecifier::Long) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    cur_bb,
                )?
            }

            (
                false,
                TypeSpecifier::Int,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong,
            )
            | (false, TypeSpecifier::Long, TypeSpecifier::UnsignedLongLong) => {
                *left_info = cast_operation(
                    *left_info,
                    right_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    cur_bb,
                )?
            }

            (
                true,
                TypeSpecifier::Int,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong,
            )
            | (true, TypeSpecifier::Long, TypeSpecifier::UnsignedLongLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::Trunc,
                    ir_state,
                    cur_bb,
                )?
            }

            // UNSIGNED TO SIGNED CONVERSION

            // We use 64 bit long and 32 bit int -> hence long can represent all values of an unsigned int.
            // Convert the unsigned int to long
            (_, TypeSpecifier::Long | TypeSpecifier::LongLong, TypeSpecifier::UnsignedInt)
            | (_, TypeSpecifier::LongLong, TypeSpecifier::UnsignedLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    cur_bb,
                )?
            }
            (false, TypeSpecifier::UnsignedInt, TypeSpecifier::Long | TypeSpecifier::LongLong)
            | (false, TypeSpecifier::UnsignedLong, TypeSpecifier::LongLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    cur_bb,
                )?
            }
            (true, TypeSpecifier::UnsignedInt, TypeSpecifier::Long | TypeSpecifier::LongLong)
            | (true, TypeSpecifier::UnsignedLong, TypeSpecifier::LongLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::Trunc,
                    ir_state,
                    cur_bb,
                )?
            }

            (_, TypeSpecifier::Int, TypeSpecifier::Int)
            | (_, TypeSpecifier::UnsignedInt, TypeSpecifier::UnsignedInt)
            | (_, TypeSpecifier::Long, TypeSpecifier::Long)
            | (_, TypeSpecifier::UnsignedLong, TypeSpecifier::UnsignedLong)
            | (_, TypeSpecifier::LongLong, TypeSpecifier::LongLong)
            | (_, TypeSpecifier::UnsignedLongLong, TypeSpecifier::UnsignedLongLong)
            | (_, TypeSpecifier::LongDouble, TypeSpecifier::LongDouble)
            | (_, TypeSpecifier::Double, TypeSpecifier::Double)
            | (_, TypeSpecifier::Float, TypeSpecifier::Float) => (),

            (_, TypeSpecifier::Void, _)
            | (_, _, TypeSpecifier::Void)
            | (_, TypeSpecifier::TypeAlias(..), _)
            | (_, _, TypeSpecifier::TypeAlias(..))
            | (_, TypeSpecifier::Enum { .. }, _)
            | (_, _, TypeSpecifier::Enum { .. })
            | (_, TypeSpecifier::Struct { .. }, _)
            | (_, _, TypeSpecifier::Struct { .. })
            | (_, TypeSpecifier::None, _)
            | (_, _, TypeSpecifier::None) => {
                panic!("Casting not implemented for {l_type_spec:?} and {r_type_spec:?}")
            }
            (_, TypeSpecifier::SignedChar, _)
            | (_, _, TypeSpecifier::SignedChar)
            | (_, TypeSpecifier::UnsignedChar, _)
            | (_, _, TypeSpecifier::UnsignedChar)
            | (_, TypeSpecifier::Short, _)
            | (_, _, TypeSpecifier::Short)
            | (_, TypeSpecifier::UnsignedShort, _)
            | (_, _, TypeSpecifier::UnsignedShort) => {
                unreachable!("We should have dealt with these types during integral promotion.")
            }
        }
    } else {
        panic!("Casting not implemented for non-primitive types")
    };

    Ok(())
    // left and right now have the same type
}
fn specifier_to_type<'s, 'a>(
    type_specifier: TypeSpecifier<'a>,
    arena: &'a Arena,
) -> Result<Type<'a>, CompilationError<'a>> {
    let specifiers = Specifiers {
        type_specifier,
        ..Default::default()
    };
    let t = Type {
        specifiers: arena.push(specifiers)?,
        derived_types: &[],
    };
    return Ok(t);
}

// Generate an expression in 3AC form. The destination of the most recent instruction contains the result.
fn ir_gen_expression<'i, 's, 'a>(
    expr: &'a Node<'a>,
    cur_bb: BlockID,
    ir_state: &'i mut IRState<'s, 'a>,
) -> Result<VariableInfo<'a>, CompilationError<'a>> {
    // Used for allocating simpy types

    // let cur_bb_data =
    match *expr {
        Node::Identifier { name } => {
            let var_info = match get_var_info(ir_state.scopes, name) {
                Some(t) => t,
                None => return Err(CompilationError::NoDefinitionFound),
            };

            let copy_reg_dst = Register {
                disp_name: Some(name),
                uuid: next_uuid(ir_state),
                version: None,
                ir_type: var_info.reg.ir_type,
            };

            let load_var_inst = Instruction::Load {
                dst_type: var_info.reg.ir_type,
                dst: copy_reg_dst,
                src_type: IRType::Ptr,
                src: var_info.reg,
            };
            ir_state.blocks[cur_bb].instructions.push(load_var_inst);

            let res_info = VariableInfo {
                c_type: var_info.c_type,
                is_lvalue: true,
                reg: copy_reg_dst,
            };

            return Ok(res_info);
        }

        // TODO: These specifier_to_type calls could all just be references to const literal types rather than allocation on the arena
        Node::Literal(literal) => {
            let arena = ir_state.arena;
            let (c_type, ir_type, constant) = match literal.token_type {
                TokenType::IntLiteral(val) => (
                    specifier_to_type(TypeSpecifier::Int, arena)?,
                    IRType::I32,
                    Constant::I32(val),
                ),
                TokenType::LongLiteral(val) => (
                    specifier_to_type(TypeSpecifier::Long, arena)?,
                    IRType::I64,
                    Constant::I64(val),
                ),
                TokenType::UIntLiteral(val) => (
                    specifier_to_type(TypeSpecifier::UnsignedInt, arena)?,
                    IRType::I32,
                    Constant::I32(i32::from_be_bytes(val.to_be_bytes())),
                ),
                TokenType::ULongLiteral(val) => (
                    specifier_to_type(TypeSpecifier::UnsignedLong, arena)?,
                    IRType::I64,
                    Constant::I64(i64::from_be_bytes(val.to_be_bytes())),
                ),
                TokenType::CharLiteral(val) => (
                    specifier_to_type(TypeSpecifier::SignedChar, arena)?,
                    IRType::I8,
                    Constant::I8(val as i8),
                ),
                TokenType::FloatLiteral(val) => (
                    specifier_to_type(TypeSpecifier::Float, arena)?,
                    IRType::F32,
                    Constant::F32(val),
                ),
                TokenType::DoubleLiteral(val) => (
                    specifier_to_type(TypeSpecifier::Double, arena)?,
                    IRType::F64,
                    Constant::F64(val),
                ),
                _ => return Err(CompilationError::InvalidASTStructure),
            };

            let dst = new_temp_reg(ir_state, ir_type);

            let src = Operand::Constant(constant);
            let instruction = Instruction::Assign { dst, src };
            ir_state.blocks[cur_bb].instructions.push(instruction);

            let res_info = VariableInfo {
                c_type,
                is_lvalue: false,
                reg: dst,
            };

            return Ok(res_info);
        }

        Node::Infix {
            operator,
            left,
            right,
        } => {
            println!("INFIX WITH {:?}", operator.token_type);
            let mut left_info = ir_gen_expression(left, cur_bb, ir_state)?;
            let mut right_info = ir_gen_expression(right, cur_bb, ir_state)?;

            // TODO: Typecheck in cast?
            cast(
                &mut left_info,
                &mut right_info,
                operator.token_type,
                ir_state,
                cur_bb,
            )?;

            // Left and right now have the same type after casting
            let ir_type = right_info.reg.ir_type;
            let left = Operand::Register(left_info.reg);
            let right = Operand::Register(right_info.reg);

            if left_info.c_type.derived_types.is_empty() == false
                || right_info.c_type.derived_types.is_empty() == false
            {
                panic!("We do not yet perform infix operations on types with derived types.");
            }
            // TODO: Support floats
            let flags = match (
                left_info.c_type.specifiers.type_specifier,
                right_info.c_type.specifiers.type_specifier,
            ) {
                (
                    TypeSpecifier::Short
                    | TypeSpecifier::Int
                    | TypeSpecifier::Long
                    | TypeSpecifier::LongLong,
                    TypeSpecifier::Short
                    | TypeSpecifier::Int
                    | TypeSpecifier::Long
                    | TypeSpecifier::LongLong,
                ) => Some(ArithmeticFlags::Nsw),
                _ => None,
            };

            match operator.token_type {
                TokenType::Plus => {
                    let dst = new_temp_reg(ir_state, ir_type);

                    // Signed integer overflow is not allowed in C90.
                    ir_state.blocks[cur_bb].instructions.push(Instruction::Add {
                        dst,
                        ir_type,
                        flags,
                        left,
                        right,
                    });
                    return Ok(VariableInfo {
                        c_type: right_info.c_type,
                        is_lvalue: false,
                        reg: dst,
                    });
                }
                TokenType::Asterisk => {
                    let dst = new_temp_reg(ir_state, ir_type);
                    ir_state.blocks[cur_bb].instructions.push(Instruction::Mul {
                        dst,
                        ir_type,
                        flags,
                        left,
                        right,
                    });
                    return Ok(VariableInfo {
                        c_type: right_info.c_type,
                        is_lvalue: false,
                        reg: dst,
                    });
                }
                TokenType::Assign => {
                    // We assume here that at generation, every variable is stored on
                    // the heap. A later optimisation pass will turn these into
                    // register SSAs.
                    if left_info.is_lvalue {
                        ir_state.blocks[cur_bb]
                            .instructions
                            .push(Instruction::Store {
                                src_type: right_info.reg.ir_type,
                                src: right,
                                dst_type: left_info.reg.ir_type,
                                dst: left_info.reg,
                            });
                        // While we require the assigment destination to be an lvalue,
                        // the result of evaluating an expression is the value of the lvalue,
                        // but this expression may not be reassigned.
                        return Ok(VariableInfo {
                            c_type: left_info.c_type,
                            is_lvalue: false,
                            reg: left_info.reg,
                        });
                    } else {
                        panic!("Attempted to assign to lvalue.")
                    }
                }
                TokenType::Equals => {
                    let dst = new_temp_reg(ir_state, ir_type);
                    let cmp_instr = Instruction::Icmp {
                        dst,
                        comparison: ComparisonKind::Eq,
                        op_type: right_info.reg.ir_type,
                        op1: right,
                        op2: left,
                    };

                    ir_state.blocks[cur_bb].instructions.push(cmp_instr);
                    return Ok(VariableInfo {
                        c_type: right_info.c_type,
                        is_lvalue: false,
                        reg: dst,
                    });
                }
                _ => panic!("Operator {operator:?} not implemented."),
            }
        }

        _ => {
            panic!("Not implemented.")
        }
    }
}

pub fn ir_gen_compound_smt<'s, 'ast: 's, 'i>(
    compound_stmt: &'ast Node<'ast>,
    mut cur_bb: BlockID,
    ir_state: &'i mut IRState<'s, 'ast>,
) -> Result<BlockID, CompilationError<'ast>> {
    push_new_scope(ir_state.scopes);

    if let Node::CompoundStatement {
        declaration_list,
        statement_list,
    } = *compound_stmt
    {
        if let Node::DeclarationList(list) = declaration_list {
            for declaration in *list {
                match declaration {
                    Node::Declaration {
                        declaration_specifiers,
                        init_declarator_list: Node::InitDeclaratorList(init_list),
                    } => {
                        for init_decl in *init_list {
                            // Generate IR for the initializer expression, save it to a temp register.
                            let mut expr_dest_info =
                                ir_gen_expression(init_decl.initializer, cur_bb, ir_state)?;

                            let declarator_name = init_decl.declarator.name.unwrap();

                            let decl_dest = if init_decl.declarator.derived_types.is_empty() {
                                Register {
                                    disp_name: Some(declarator_name),
                                    uuid: next_uuid(ir_state),
                                    version: None,
                                    ir_type: IRType::Ptr,
                                }
                            } else {
                                panic!("Not implemented.")
                            };

                            // Allocate a stack space for the register.
                            // This avoids dealing with SSA restrictions
                            let alloc_instr = Instruction::Alloca {
                                dst_type: expr_dest_info.reg.ir_type,
                                dst: decl_dest,
                            };
                            ir_state.blocks[cur_bb].instructions.push(alloc_instr);

                            let mut decl_info = VariableInfo {
                                reg: decl_dest,
                                is_lvalue: true,
                                c_type: Type {
                                    derived_types: init_decl.declarator.derived_types,
                                    specifiers: declaration_specifiers,
                                },
                            };

                            cast(
                                &mut decl_info,
                                &mut expr_dest_info,
                                TokenType::Assign,
                                ir_state,
                                cur_bb,
                            )?;

                            // Store the temp register in the stack space for the actual register.
                            let store_instr = Instruction::Store {
                                src_type: expr_dest_info.reg.ir_type,
                                src: Operand::Register(expr_dest_info.reg),
                                dst_type: decl_dest.ir_type,
                                dst: decl_dest,
                            };
                            ir_state.blocks[cur_bb].instructions.push(store_instr);

                            push_var_type(
                                ir_state.scopes,
                                init_decl.declarator.name.unwrap(),
                                decl_info,
                            )?;
                        }
                    }
                    _ => return Err(CompilationError::InvalidASTStructure),
                }
            }
        }
        if let Node::StatementList(list) = statement_list {
            for statement in *list {
                match statement {
                    // Select statements
                    Node::IfStatementList(list) => {
                        let bb_after_statements = get_next_block(ir_state.blocks);
                        let mut if_iter = (*list).iter().peekable();
                        while let Some(if_statement) = if_iter.next() {
                            // Basic block that (may) come after the if statement
                            if let Node::Empty = if_statement.condition {
                                // TODO: This is incorrect
                                ir_gen_compound_smt(if_statement.statement, cur_bb, ir_state)?;
                            } else {
                                let cond_info =
                                    ir_gen_expression(if_statement.condition, cur_bb, ir_state)?;
                                let cond_reg = cond_info.reg;

                                let cmp_dest = new_temp_reg(ir_state, IRType::I1);
                                let cmp_instr = Instruction::Icmp {
                                    dst: cmp_dest,
                                    comparison: ComparisonKind::Eq,
                                    op_type: cond_reg.ir_type,
                                    op1: Operand::Register(cond_reg),
                                    op2: Operand::Constant(int_to_constant(0, cond_reg.ir_type)),
                                };

                                let blocks = &mut ir_state.blocks;
                                blocks[cur_bb].instructions.push(cmp_instr);

                                let mut statement_bb = get_next_block(blocks);

                                let fallthrough_bb = if if_iter.peek().is_none() {
                                    bb_after_statements
                                } else {
                                    get_next_block(blocks)
                                };

                                let br_cond_term = Terminator::Brcond {
                                    condition: Operand::Register(cmp_dest),
                                    if_true_dst: statement_bb,
                                    if_false_dst: fallthrough_bb,
                                };
                                blocks[cur_bb].terminator = Some(br_cond_term);
                                cur_bb = fallthrough_bb;

                                statement_bb = ir_gen_compound_smt(
                                    if_statement.statement,
                                    statement_bb,
                                    ir_state,
                                )?;

                                let statement_br = Terminator::Br {
                                    dst: bb_after_statements,
                                };

                                ir_state.blocks[statement_bb].terminator = Some(statement_br);

                                // cur_bb.successors.push(statement_first_bb);

                                // if first_bb.is_none() {
                                //     first_bb = Some(ir_state.blocks.len());
                                //     // ir_state.blocks.push(cur_bb);
                                //     // cur_bb = &mut new_bb();
                                // }

                                // let cur_bb_id = ir_state.blocks.len() - 1;

                                // let statement_successors =
                                //     &mut ir_state.blocks[statement_last_bb].successors;
                                // if statement_successors.is_empty() {
                                //     statement_successors.push(cur_bb_id);
                                // }

                                // let statement_bb = ir_state.blocks.last();
                            }

                            //bb.successors

                            // bb.instructions
                        }
                        cur_bb = bb_after_statements;
                    }
                    Node::CompoundStatement { .. } => {
                        ir_gen_compound_smt(statement, cur_bb, ir_state)?;
                    }
                    // Iteration statements
                    Node::WhileStatement { .. } => return Err(CompilationError::NotImplemented),
                    Node::DoWhileStatement { .. } => return Err(CompilationError::NotImplemented),
                    Node::ForStatement { .. } => return Err(CompilationError::NotImplemented),

                    // Jump statements.
                    Node::Goto { .. } => return Err(CompilationError::NotImplemented),
                    Node::Return { .. } => return Err(CompilationError::NotImplemented),

                    _ => {
                        ir_gen_expression(statement, cur_bb, ir_state)?;
                    }
                }
            }
        }
    }

    pop_scope(ir_state.scopes);

    // let last_bb = ir_state.blocks.len();
    // TODO: Better handlling of pushing new BB's/checking they have 1 instruction or more

    // ir_state.blocks.push(arena.push(cur_bb)?);
    return Ok(cur_bb);
}

pub fn ir_gen_translation_unit<'s, 'ast: 's>(
    translation_unit: &'ast Node<'ast>,
    arena: &'ast Arena,
) -> Result<(), CompilationError<'ast>> {
    let cur_bb_data = new_bb();
    let mut cur_bb = 0;
    // let mut blocks = vec![cur_bb_data];

    let mut ir_state = IRState {
        uuid_count: 0,
        blocks: &mut vec![cur_bb_data],
        scopes: &mut Scopes::new(),
        arena,
    };
    // Global Scope
    push_new_scope(ir_state.scopes);

    if let Node::TranslationalUnit {
        external_declarations,
    } = *translation_unit
    {
        for external_declaration in external_declarations {
            match *external_declaration {
                Node::FunctionDefinition {
                    declaration_specifiers,
                    declarator,
                    compound_statement,
                } => {
                    let func_type: Type = Type {
                        specifiers: declaration_specifiers,
                        derived_types: declarator.derived_types,
                    };
                    // push_var_type(scopes, declarator.name.unwrap(), TypeInfo { c_type :func_type, )?;
                    cur_bb = ir_gen_compound_smt(compound_statement, cur_bb, &mut ir_state)?;
                }

                /*
                Node::Declaration { declaration_specifiers, init_declarator_list : Node::InitDeclaratorList(list) } => {
                    if declaration_specifiers.is_typedef {
                        continue;
                    }

                    for init_declarator in *list {
                        let declarator = init_declarator.declarator;
                        let declaration_type = Type{ specifiers : declaration_specifiers, derived_types : declarator.derived_types};
                        push_variable(scopes, declarator.name.unwrap(), declaration_type)?;
                    }
                }*/
                _ => return Err(CompilationError::InvalidASTStructure),
            }
        }

        // println!("{:?}", ir_state);
        for (i, bb) in ir_state.blocks.iter().enumerate() {
            println!("\n\nBB {i}: ");
            print_basic_block(&bb);
        }
        Ok(())
    } else {
        return Err(CompilationError::InvalidASTStructure);
    }
}
