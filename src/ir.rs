use core::panic;
use std::collections::HashMap;
use std::println;
use crate::arena::*;
use crate::compile::CompilationError;
use crate::typechecker::*; 
use crate::parser::*;
use crate::lexer::*;

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
pub struct Variable<'v> {
    disp_name : Option<&'v str>,
    
    uuid : u32,
    version: Option<u32>,
    ir_type : IRType,
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
    Variable(Variable<'o>),
    Constant(Constant),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Instruction<'i> {
    Add { ir_type : IRType, dst : Variable<'i>, left : Operand<'i>, right: Operand<'i>},
    SignExtend { dst : Variable<'i>, src : Operand<'i>, src_type : IRType, dst_type : IRType},
    SiToFp { dst : Variable<'i>, src : Operand<'i>, src_type : IRType, dst_type : IRType},
    Assign { dst: Variable<'i>, src: Operand<'i>},
    FunctionCall {dst : Variable<'i>, parameters : &'i [&'i Variable<'i>]}
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicBlock<'b> {
    instructions : Vec<Instruction<'b>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct IRState<'i> {
    uuid_count : u32,
    blocks : Vec<BasicBlock<'i>>
}

fn print_basic_block(bb : &BasicBlock) {
   for instruction in &bb.instructions {
        match instruction {
            Instruction::Add { ir_type, dst, left, right} => println!("{dst:?} = Add {ir_type:?}, {left:?} {right:?}"),
            Instruction::Assign { dst, src} => println!("{dst:?} = {src:?}"),
            Instruction::SignExtend { dst, src, src_type, dst_type } => println!("{dst:?} = S. Ext {src:?} {src_type:?} to {dst_type:?}"),
            _ => println!("Print not implemented for {:?}", instruction),
        }
    } 
}

fn specifier_to_ir_type(type_specifier : &TypeSpecifier) -> IRType {
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

        _ => panic!("Not implemented this type specifier to IR conversion.")
    }
}

fn new_temp_var<'s, 'a : 's>(ir_state : &'s mut IRState<'a>, ir_type : IRType) -> Variable<'a> {
    ir_state.uuid_count += 1;
    Variable { disp_name : None, uuid : ir_state.uuid_count, version : None, ir_type}
}

// Generate an expression in 3AC form. The destination of the most recent instruction contains the result.
fn ir_gen_expression<'a, 's>(expr : &'a Node<'a>, scopes : &Scopes<'s, 's>, bb : &'s mut BasicBlock<'a>, ir_state : &'s mut IRState<'a>, arena : &'a Arena) -> Result<(ExprType<'a>, Variable<'a>), CompilationError<'a>>
{
    
    let specifier_to_expr_type = |type_specifier| {
        let specifiers = Specifiers { type_specifier, ..Default::default() };
        return ExprType{ specifiers, ..Default::default()}
    };
    
    match *expr {
        Node::Literal(literal) => {
            let (c_type, ir_type, constant) = match literal.token_type {
                TokenType::IntLiteral(val) => (specifier_to_expr_type(TypeSpecifier::Int), IRType::I32, Constant::I32(val)),
                TokenType::LongLiteral(val) => (specifier_to_expr_type(TypeSpecifier::Short), IRType::I64, Constant::I64(val)),
                TokenType::UIntLiteral(val) => (specifier_to_expr_type(TypeSpecifier::UnsignedInt), IRType::I32, Constant::I32(i32::from_be_bytes(val.to_be_bytes()))),
                TokenType::ULongLiteral(val) => (specifier_to_expr_type(TypeSpecifier::UnsignedLong), IRType::I64, Constant::I64(i64::from_be_bytes(val.to_be_bytes()))),
                TokenType::CharLiteral(val) => (specifier_to_expr_type(TypeSpecifier::SignedChar), IRType::I8, Constant::I8(val as i8)),
                TokenType::FloatLiteral(val) => (specifier_to_expr_type(TypeSpecifier::Float), IRType::F32, Constant::F32(val)),
                TokenType::DoubleLiteral(val) => (specifier_to_expr_type(TypeSpecifier::Double), IRType::F64, Constant::F64(val)),
                _ => return Err(CompilationError::InvalidASTStructure),
            };

            let dst = Variable { disp_name : None, uuid : ir_state.uuid_count, version : None, ir_type};
            ir_state.uuid_count += 1;

            let src = Operand::Constant(constant);
            let instruction = Instruction::Assign { dst, src };
            bb.instructions.push(instruction);
            return Ok((c_type, dst))
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

        // A char, a short int, or an int bit-field, or their signed or unsigned varieties, or an object that has enumeration type, 
        // may be used in an expression wherever an int or unsigned int may be used. 
        // If an int can represent all values of the original type, the value is converted to an int; 
        // otherwise it is converted to an unsigned int. These are called the integral promotions
        Node::Infix { operator, left, right } => {
            
            let (mut left_c_type, mut left_variable) = ir_gen_expression(left, scopes, bb, ir_state, arena)?;
            let (mut right_c_type, mut right_variable) = ir_gen_expression(right, scopes, bb, ir_state, arena)?;

            let (c_type, ir_type) = if left_c_type.derived_types.is_empty() && right_c_type.derived_types.is_empty() {
                // Both left and right are arithmetic, perform type promotion
        
                let l_type_spec = left_c_type.specifiers.type_specifier;
                let r_type_spec = right_c_type.specifiers.type_specifier;

                enum PromotionDirection {
                    LeftToRight,
                    RightToLeft
                }
                use PromotionDirection::*;

                // TODO: This currently only works for integers.
                // How do we handle
                let mut promote = |direction : PromotionDirection| {
                    let (src_type_spec, dst_type_spec, src_var) = match direction {
                        PromotionDirection::LeftToRight => (l_type_spec, r_type_spec, left_variable),                        
                        PromotionDirection::RightToLeft => (r_type_spec, l_type_spec, right_variable)
                    };
                    let src_type = specifier_to_ir_type(&src_type_spec);
                    let dst_type = specifier_to_ir_type(&dst_type_spec);
                    println!("{src_type:?} {dst_type:?}");
                    let dst = new_temp_var(ir_state, dst_type);
                    let cast_instr = Instruction::SignExtend { dst, src: Operand::Variable(src_var), src_type, dst_type };
                    bb.instructions.push(cast_instr);
                    match direction {
                        PromotionDirection::LeftToRight => left_variable = dst,
                        PromotionDirection::RightToLeft => right_variable = dst,       
                    }
                    (specifier_to_expr_type(dst_type_spec), dst_type)
                };

                
                match (l_type_spec, r_type_spec) {
                    (TypeSpecifier::LongDouble, _) => promote(RightToLeft), 
                    (_, TypeSpecifier::LongDouble) => promote(LeftToRight),
                    
                    (TypeSpecifier::Double, _) => promote(RightToLeft),
                    (_, TypeSpecifier::Double) => promote(LeftToRight), 
                    
                    (TypeSpecifier::Float, _) => promote(RightToLeft), 
                    (_, TypeSpecifier::Float) => promote(LeftToRight),

                    (TypeSpecifier::UnsignedLong, _) => promote(RightToLeft),
                    (_, TypeSpecifier::UnsignedLong) => promote(LeftToRight),
                    // We use 64 bit long and 32 bit int -> hence long can represent all values of an unsigned int.
                    // Convert the unsigned int to long
                    (TypeSpecifier::Long,TypeSpecifier::UnsignedInt) => promote(RightToLeft),
                    (TypeSpecifier::UnsignedInt, TypeSpecifier::Long) => promote(LeftToRight),
                    
                    (TypeSpecifier::Int, TypeSpecifier::Int) => (specifier_to_expr_type(TypeSpecifier::Int), IRType::I32),
                    _ => return Err(CompilationError::NotImplemented)
                }
            } else {
                return Err(CompilationError::NotImplemented);
            };

            let dst = new_temp_var(ir_state, ir_type);
            let instruction = match operator.token_type {
                TokenType::Plus => Instruction::Add { ir_type, dst, left: Operand::Variable(left_variable), right: Operand::Variable(right_variable) },
                _ => panic!("Not implemented."),
            };
            
            bb.instructions.push(instruction);
            return Ok((c_type, dst))
        }

        _ => {
            return Err(CompilationError::InvalidASTStructure);
        }
    }
}

pub fn ir_gen_compound_smt<'s, 'ast : 's>(compound_stmt : &'ast Node<'ast>, type_scopes : &'s mut Scopes<'ast, 'ast>, ir_state : &'s mut IRState<'ast>, arena : &'ast Arena) -> Result<(), CompilationError<'ast>>{
    push_new_scope(type_scopes);
    let instructions = Vec::<Instruction>::new();
    let mut bb = BasicBlock{ instructions};

    if let Node::CompoundStatement { declaration_list, statement_list } = *compound_stmt {
         
        if let Node::DeclarationList(list) = declaration_list {
            for declaration in *list {

                match declaration {    
                    Node::Declaration { declaration_specifiers, init_declarator_list : Node::InitDeclaratorList(init_list) } => {
                        for init_decl in *init_list {
                            ir_gen_expression(init_decl.initializer, type_scopes, &mut bb, ir_state, arena)?;
                            
                            let var_type = Type{ specifiers : declaration_specifiers, derived_types : init_decl.declarator.derived_types};
                            push_var_type(type_scopes, init_decl.declarator.name.unwrap(), var_type)?;


                            
                        }
                    }
                    _ => return Err(CompilationError::InvalidASTStructure)
                }
            }


        } else {
            return Err(CompilationError::InvalidASTStructure);
        }
    }

    print_basic_block(&bb);
    ir_state.blocks.push(bb);

    pop_scope(type_scopes);
    return Ok(())
}

pub fn ir_gen_translation_unit<'s, 'ast : 's>(translation_unit : &'ast Node<'ast>, arena : &'ast Arena) -> Result<(), CompilationError<'ast>>{

    let scopes = &mut Scopes::new();
    // Global Scope
    push_new_scope(scopes);

    let mut ir_state = IRState { uuid_count : 0, blocks : Vec::new()};
    
    if let Node::TranslationalUnit { external_declarations } = *translation_unit {
        for external_declaration in external_declarations {
            match *external_declaration {

                Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
                    let func_type : Type = Type{specifiers : declaration_specifiers, derived_types : declarator.derived_types};
                    push_var_type(scopes, declarator.name.unwrap(), func_type)?;                   
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

        println!("{:?}", ir_state);

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
