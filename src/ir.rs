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
    Bool,
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
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    //F128(f128),
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
    SiToFp {
        dst: Register<'i>,
        src: Operand<'i>,
        src_type: IRType,
        dst_type: IRType,
    },
    Assign {
        dst: Register<'i>,
        src: Operand<'i>,
    },
    _FunctionCall {
        dst: Register<'i>,
        parameters: &'i [&'i Register<'i>],
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
            _ => return write!(f, "{FORMAT_END}Display unimpl. for {self:?}"),
        }
        write!(f, "{FORMAT_END}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock<'b> {
    instructions: Vec<Instruction<'b>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IRState<'i> {
    uuid_count: u32,
    blocks: Vec<BasicBlock<'i>>,
}

fn print_basic_block(bb: &BasicBlock) {
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
            _ => println!("Print not implemented for {:?}", instruction),
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

fn next_uuid<'a>(ir_state: &mut IRState<'a>) -> u32 {
    ir_state.uuid_count += 1;
    return ir_state.uuid_count;
}

fn new_temp_reg<'s, 'a: 's>(ir_state: &'s mut IRState<'a>, ir_type: IRType) -> Register<'a> {
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

fn cast_operation<'s, 'a>(
    from_info: VariableInfo<'a>,
    to_type: Type<'a>,
    cast_op: CastOp,
    ir_state: &'s mut IRState<'a>,
    bb: &'s mut BasicBlock<'a>,
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
    bb.instructions.push(cast_instr);

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

fn integral_promotion<'s, 'a>(
    var_info: &VariableInfo<'a>,
    ir_state: &'s mut IRState<'a>,
    bb: &'s mut BasicBlock<'a>,
) -> Result<VariableInfo<'a>, CompilationError<'a>> {
    match var_info.c_type.specifiers.type_specifier {
        TypeSpecifier::SignedChar
        | TypeSpecifier::UnsignedChar
        | TypeSpecifier::Short
        | TypeSpecifier::UnsignedShort => {
            cast_operation(*var_info, INT_TYPE, CastOp::SignExtend, ir_state, bb)
        }
        _ => Ok(*var_info),
    }
}

fn cast<'s, 'a>(
    left_info: &mut VariableInfo<'a>,
    right_info: &mut VariableInfo<'a>,
    operator: TokenType,
    ir_state: &'s mut IRState<'a>,
    bb: &'s mut BasicBlock<'a>,
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
        *right_info = integral_promotion(&right_info, ir_state, bb)?;

        if let TokenType::Assign = operator {
            ()
        } else {
            *left_info = integral_promotion(&left_info, ir_state, bb)?;
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
                    bb,
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
                    bb,
                )?
            }

            (
                true,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedInt,
                TypeSpecifier::UnsignedLongLong,
            )
            | (true, TypeSpecifier::UnsignedInt, TypeSpecifier::UnsignedLong) => {
                *right_info =
                    cast_operation(*right_info, left_info.c_type, CastOp::Trunc, ir_state, bb)?
            }

            // SIGNED INTEGER PROMOTIONS
            (_, TypeSpecifier::LongLong, TypeSpecifier::Long | TypeSpecifier::Int)
            | (_, TypeSpecifier::Long, TypeSpecifier::Int) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::SignExtend,
                    ir_state,
                    bb,
                )?
            }

            (false, TypeSpecifier::Long | TypeSpecifier::Int, TypeSpecifier::LongLong)
            | (false, TypeSpecifier::Int, TypeSpecifier::Long) => {
                *left_info = cast_operation(
                    *left_info,
                    right_info.c_type,
                    CastOp::SignExtend,
                    ir_state,
                    bb,
                )?
            }

            (true, TypeSpecifier::Long | TypeSpecifier::Int, TypeSpecifier::LongLong)
            | (true, TypeSpecifier::Int, TypeSpecifier::Long) => {
                *right_info =
                    cast_operation(*right_info, left_info.c_type, CastOp::Trunc, ir_state, bb)?
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
                *right_info =
                    cast_operation(*right_info, left_info.c_type, CastOp::Trunc, ir_state, bb)?
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
                    bb,
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
                    bb,
                )?
            }

            (
                true,
                TypeSpecifier::Int,
                TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong,
            )
            | (true, TypeSpecifier::Long, TypeSpecifier::UnsignedLongLong) => {
                *right_info =
                    cast_operation(*right_info, left_info.c_type, CastOp::Trunc, ir_state, bb)?
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
                    bb,
                )?
            }
            (false, TypeSpecifier::UnsignedInt, TypeSpecifier::Long | TypeSpecifier::LongLong)
            | (false, TypeSpecifier::UnsignedLong, TypeSpecifier::LongLong) => {
                *right_info = cast_operation(
                    *right_info,
                    left_info.c_type,
                    CastOp::ZeroExtend,
                    ir_state,
                    bb,
                )?
            }
            (true, TypeSpecifier::UnsignedInt, TypeSpecifier::Long | TypeSpecifier::LongLong)
            | (true, TypeSpecifier::UnsignedLong, TypeSpecifier::LongLong) => {
                *right_info =
                    cast_operation(*right_info, left_info.c_type, CastOp::Trunc, ir_state, bb)?
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
fn ir_gen_expression<'s, 'a>(
    expr: &'a Node<'a>,
    scopes: &Scopes<'s, 'a>,
    bb: &'s mut BasicBlock<'a>,
    ir_state: &'s mut IRState<'a>,
    arena: &'a Arena,
) -> Result<VariableInfo<'a>, CompilationError<'a>> {
    // Used for allocating simpy types

    match *expr {
        Node::Identifier { name } => {
            let var_info = match get_var_info(scopes, name) {
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
            bb.instructions.push(load_var_inst);

            let res_info = VariableInfo {
                c_type: var_info.c_type,
                is_lvalue: true,
                reg: copy_reg_dst,
            };

            return Ok(res_info);
        }

        // TODO: These specifier_to_type calls could all just be references to const literal types rather than allocation on the arena
        Node::Literal(literal) => {
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
            bb.instructions.push(instruction);

            let res_info = VariableInfo {
                c_type,
                is_lvalue: false,
                reg: dst,
            };

            return Ok(res_info);
        }

        // First, if either operand has type long double, the other operand is converted to long double .
        // Otherwise, if either operand has type double, the other operand is converted to double.
        // Otherwise, if either operand has type float, the other operand is converted to float.
        // Otherwise, the integral promotions are performed on both operands.
        // Then the following rules are applied:
        // If either operand has type unsigned long int, the other operand is converted to unsigned long int.
        // Otherwise, if one operand has type long int and the other has type unsigned int, if a long int can
        // represent all values of an unsigned int, the operand of type unsigned int is converted to long int
        // ; if a long int cannot represent all the values of an unsigned int, both operands are converted to unsigned long int.
        // Otherwise, if either operand has type long int, the other operand is converted to long int.
        // Otherwise, if either operand has type unsigned int, the other operand is converted to unsigned int.
        // Otherwise, both operands have type int.

        // TODO: Char/Short promotion
        // A char, a short int, or an int bit-field, or their signed or unsigned varieties, or an object that has enumeration type,
        // may be used in an expression wherever an int or unsigned int may be used.
        // If an int can represent all values of the original type, the value is converted to an int;
        // otherwise it is converted to an unsigned int. These are called the integral promotions
        Node::Infix {
            operator,
            left,
            right,
        } => {
            println!("INFIX WITH {:?}", operator.token_type);
            let mut left_info = ir_gen_expression(left, scopes, bb, ir_state, arena)?;
            let mut right_info = ir_gen_expression(right, scopes, bb, ir_state, arena)?;

            // TODO: Typecheck in cast?
            cast(
                &mut left_info,
                &mut right_info,
                operator.token_type,
                ir_state,
                bb,
            )?;

            // left and right now have the same type after casting
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
                    bb.instructions.push(Instruction::Add {
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
                    bb.instructions.push(Instruction::Mul {
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
                        bb.instructions.push(Instruction::Store {
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
                _ => panic!("Operator {operator:?} not implemented."),
            }
        }

        _ => {
            panic!("Not implemented.")
        }
    }
}

pub fn ir_gen_compound_smt<'s, 'ast: 's>(
    compound_stmt: &'ast Node<'ast>,
    type_scopes: &'s mut Scopes<'ast, 'ast>,
    ir_state: &'s mut IRState<'ast>,
    arena: &'ast Arena,
) -> Result<(), CompilationError<'ast>> {
    push_new_scope(type_scopes);
    let instructions = Vec::<Instruction>::new();
    let mut bb = BasicBlock { instructions };

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
                            let mut expr_dest_info = ir_gen_expression(
                                init_decl.initializer,
                                type_scopes,
                                &mut bb,
                                ir_state,
                                arena,
                            )?;

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
                            bb.instructions.push(alloc_instr);

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
                                &mut bb,
                            )?;

                            // Store the temp register in the stack space for the actual register.
                            let store_instr = Instruction::Store {
                                src_type: expr_dest_info.reg.ir_type,
                                src: Operand::Register(expr_dest_info.reg),
                                dst_type: decl_dest.ir_type,
                                dst: decl_dest,
                            };
                            bb.instructions.push(store_instr);

                            push_var_type(
                                type_scopes,
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
                    Node::IfStatementList(..) => return Err(CompilationError::NotImplemented),
                    Node::CompoundStatement { .. } => {
                        ir_gen_compound_smt(statement, type_scopes, ir_state, arena)?
                    }
                    // Iteration statements
                    Node::WhileStatement { .. } => return Err(CompilationError::NotImplemented),
                    Node::DoWhileStatement { .. } => return Err(CompilationError::NotImplemented),
                    Node::ForStatement { .. } => return Err(CompilationError::NotImplemented),

                    // Jump statements.
                    Node::Goto { .. } => return Err(CompilationError::NotImplemented),
                    Node::Return { .. } => return Err(CompilationError::NotImplemented),

                    _ => {
                        ir_gen_expression(statement, type_scopes, &mut bb, ir_state, arena)?;
                    }
                }
            }
        }
    }

    print_basic_block(&bb);
    ir_state.blocks.push(bb);

    pop_scope(type_scopes);
    return Ok(());
}

pub fn ir_gen_translation_unit<'s, 'ast: 's>(
    translation_unit: &'ast Node<'ast>,
    arena: &'ast Arena,
) -> Result<(), CompilationError<'ast>> {
    let scopes = &mut Scopes::new();
    // Global Scope
    push_new_scope(scopes);

    let mut ir_state = IRState {
        uuid_count: 0,
        blocks: Vec::new(),
    };

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
                    ir_gen_compound_smt(compound_statement, scopes, &mut ir_state, arena)?;
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

        Ok(())
    } else {
        return Err(CompilationError::InvalidASTStructure);
    }
}
/*
pub fn ast_to_3ac(){

    // TODO: This "pyramid of doom" could probably be circumvented by using more specialised structs in the parser
    if let Node::CompoundStatement { declaration_list, statement_list } = *compound_stmt {

        if let Node::DeclarationList(list) = declaration_list {
            for declaration in *list {
                if let Node::Declaration { declaration_specifiers, init_declarator_list } = *declaration {
                    // TODO: Here we are assuming all type declarations are of type integer.

                    if let Node::InitDeclaratorList(init_declarator) = *init_declarator_list {
                        //let reg = add_var_reg(scope_stack, init_declarator.declarator.name);
                        //codegen_expr(scope_stack, reg, &mut block.instructions, init_declarator.initializer)?;
                    } else {
                        return Err(CompilationError::NotImplemented);
                    }
                }
            }


        } else {
            return Err(CompilationError::InvalidASTStructure);
        }


        if let Node::StatementList(list) = statement_list {
            for statement in *list {
                match statement {
                    Node::Infix { operator, left, right } => {
                        match operator.token_type {
                            TokenType::Assign => {
                                match left {
                                    Node::Identifier { name } => {
                                        let reg = step_var_reg(scope_stack, name);
                                        codegen_expr(scope_stack, reg, &mut block.instructions, right)?;
                                    }

                                    Node::Prefix(_) | Node::Postfix(_) => return Err(CompilationError::UnimplVerbose("Pre- and postfix".to_string())),
                                    _ => return Err(CompilationError::InvalidASTStructure),
                                }
                            }

                            TokenType::PlusAssign | TokenType::MinusAssign | TokenType::MultAssign | TokenType::DivAssign |
                            TokenType::ModAssign |  TokenType::ANDAssign | TokenType::ORAssign | TokenType::XORAssign => {
                                return Err(CompilationError::UnimplVerbose("Codegen for extra Assignments".to_string()));
                            }

                            _ => return Err(CompilationError::InvalidASTStructure),

                        }
                    }

                    _ => return Err(CompilationError::InvalidASTStructure),
                }
            }
        }

        return Ok(block);

    } else {
        return Err(CompilationError::InvalidASTStructure);
    }
}
*/

/*
use crate::compile::CompilationError;
use crate::lexer::*;
use crate::parser::*;

#[derive(Debug)]
enum Reg<'r> {
    Zero,
    // registers associated with a variable in some way
    // functionally, these are the same as a temp register
    Var(VarReg<'r>),
}

#[derive(Debug, Clone, Copy)]
struct VarReg<'r>{
    name : &'r str,
    version : u32,
}

impl<'r> VarReg<'r> {
    fn new(name : &'r str, version : u32) -> Self{
        VarReg{
            name,
            version
        }
    }
}

type RegScope<'s> = HashMap<&'s str, u32>;

struct RegScopeStack<'s>{
    stack : Vec<RegScope<'s>>,
    temp_count : u32,
}

impl<'s> RegScopeStack<'s> {
    fn new() -> Self{
        RegScopeStack {
            stack : Vec::<RegScope>::new(),
            temp_count : 0,
        }
    }
}

fn push_reg_scope(scope_stack : &mut RegScopeStack){
    scope_stack.stack.push(RegScope::new());
}

fn get_temp_reg<'s, 'n : 's>(scope_stack : &'s mut RegScopeStack<'n>) -> VarReg<'n>{
    println!("Adding tmp register with id {}", scope_stack.temp_count);
    let temp_reg = VarReg::new("tmp", scope_stack.temp_count);
    (*scope_stack).temp_count += 1;
    return temp_reg;
}

fn add_var_reg<'s, 'n : 's>(scope_stack : &'s mut RegScopeStack<'n>, name : &'n str) -> VarReg<'n>{
    let var_reg = VarReg::new(name, 0);
    let cur_scope = scope_stack.stack.last_mut().unwrap();
    println!("adding new var register with name {name}");
    cur_scope.insert(name, var_reg.version);

    return var_reg;
}

fn step_var_reg<'s, 'n : 's>(scope_stack : &'s mut RegScopeStack<'n>, name : &'n str) -> VarReg<'n>{
    let cur_scope = scope_stack.stack.last_mut().unwrap();
    let cur_version = cur_scope.get_mut(name).unwrap();
    *cur_version += 1;

    return VarReg::new(name, *cur_version);
}

#[derive(Debug)]
enum Instruction<'i> {
    Add     { dst : VarReg<'i>, r1 : Reg<'i>, r2 : Reg<'i>},
    AddImm  { dst : VarReg<'i>, r1 : Reg<'i>, imm : i32},
}

#[derive(Debug)]
struct BasicBlock<'b>{
    instructions : Vec<Instruction<'b>>,
}

impl<'b> BasicBlock<'b> {
    fn new() -> Self {
        BasicBlock {
            instructions : Vec::<Instruction>::new(),
        }
    }
}

fn codegen_expr<'s, 'n : 's, 'i : 's>(scope_stack : &'s mut RegScopeStack<'n>, dest_reg : VarReg<'n>, instructions : &'i mut Vec<Instruction<'n>>, expr : &'n Node<'n>) -> Result<(), CompilationError<'n>>{
    match expr {
        Node::Literal(lit) => {
            if let TokenType::IntLiteral(val) = lit.token_type {
                let instr = Instruction::AddImm{ dst : dest_reg, r1 : Reg::Zero, imm : val as i32};
                instructions.push(instr);
                return Ok(())
            } else {
                return Err(CompilationError::NotImplemented);
            }
        }

        Node::Infix { operator, left, right } => {
            let left_dest = get_temp_reg(scope_stack);
            codegen_expr(scope_stack, left_dest, instructions, left)?;

            let right_dest = get_temp_reg(scope_stack);
            codegen_expr(scope_stack, right_dest, instructions, right)?;

            match operator.token_type {
                TokenType::Plus => {
                    let instr = Instruction::Add { dst: dest_reg, r1: Reg::Var(left_dest), r2: Reg::Var(right_dest) };
                    instructions.push(instr);
                }

                TokenType::Minus | TokenType::Asterisk | TokenType::Div | TokenType::Mod | TokenType::OR | TokenType::Ampersand | TokenType::XOR |
                TokenType::ORLogical | TokenType::ANDLogical | TokenType::LeftShift | TokenType::RightShift
                    => return Err(CompilationError::UnimplVerbose(format!("Codegen for infix {:?}", operator))),

                _ => return Err(CompilationError::InvalidASTStructure),
            }

            return Ok(());
        }

        _ => return Err(CompilationError::InvalidASTStructure),
    }
}

fn codegen_compound_stmt<'s, 'n : 's>(scope_stack : &'s mut RegScopeStack<'n>, compound_stmt : &'n Node<'n>) -> Result<BasicBlock<'n>, CompilationError<'n>> {
    push_reg_scope(scope_stack);

    let mut block = BasicBlock::new();

    // TODO: This "pyramid of doom" could probably be circumvented by using more specialised structs in the parser
    if let Node::CompoundStatement { declaration_list, statement_list } = *compound_stmt {

        if let Node::DeclarationList(list) = declaration_list {
            for declaration in *list {
                if let Node::Declaration { declaration_specifiers, init_declarator_list } = *declaration {
                    // TODO: Here we are assuming all type declarations are of type integer.

                    if let Node::InitDeclaratorList(init_declarator) = *init_declarator_list {
                        //let reg = add_var_reg(scope_stack, init_declarator.declarator.name);
                        //codegen_expr(scope_stack, reg, &mut block.instructions, init_declarator.initializer)?;
                    } else {
                        return Err(CompilationError::NotImplemented);
                    }
                }
            }


        } else {
            return Err(CompilationError::InvalidASTStructure);
        }


        if let Node::StatementList(list) = statement_list {
            for statement in *list {
                match statement {
                    Node::Infix { operator, left, right } => {
                        match operator.token_type {
                            TokenType::Assign => {
                                match left {
                                    Node::Identifier { name } => {
                                        let reg = step_var_reg(scope_stack, name);
                                        codegen_expr(scope_stack, reg, &mut block.instructions, right)?;
                                    }

                                    Node::Prefix(_) | Node::Postfix(_) => return Err(CompilationError::UnimplVerbose("Pre- and postfix".to_string())),
                                    _ => return Err(CompilationError::InvalidASTStructure),
                                }
                            }

                            TokenType::PlusAssign | TokenType::MinusAssign | TokenType::MultAssign | TokenType::DivAssign |
                            TokenType::ModAssign |  TokenType::ANDAssign | TokenType::ORAssign | TokenType::XORAssign => {
                                return Err(CompilationError::UnimplVerbose("Codegen for extra Assignments".to_string()));
                            }

                            _ => return Err(CompilationError::InvalidASTStructure),

                        }
                    }

                    _ => return Err(CompilationError::InvalidASTStructure),
                }
            }
        }

        return Ok(block);

    } else {
        return Err(CompilationError::InvalidASTStructure);
    }
}

/*
fn codegen_function<'n>(function_definition : &'n Node<'n>) -> Result<(), CompilationError<'n>>{


    Ok(())
}*/


pub fn codegen_start<'n>(translation_unit : &'n Node) -> Result<(), CompilationError<'n>>{

    let scope_stack = &mut RegScopeStack::new();
    // Global Scope
    push_reg_scope(scope_stack);

    if let Node::TranslationalUnit { external_declarations } = *translation_unit {
        for external_declaration in external_declarations {
            match *external_declaration {

                Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
                    let basic_block = codegen_compound_stmt(scope_stack, &compound_statement)?;
                    println!("{:?}", basic_block);
                    return Ok(());
                }

                /*
                Node::Declaration { declaration_specifiers, init_declarator_list } => {

                    // Just a a single declarator
                    if let Node::InitDeclarator { declarator, initializer } = init_declarator_list {
                        let declaration_type = Type{ specifiers : declaration_specifiers.specifiers, derived_types : declarator.derived_types};
                    } else {
                        // TODO: Implement initializer lists
                        return Err(CompilationError::NotImplemented);
                    }
                }*/
                _ => return Err(CompilationError::InvalidASTStructure),
            }
        }

        Ok(())
    } else {
        return Err(CompilationError::InvalidASTStructure);
    }

}*/
