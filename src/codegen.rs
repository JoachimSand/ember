

// Internal IR

use std::collections::HashMap;

/*

// Allocate memory on stack frame of current function
// Push down the stack frame, essentially
alloca [Type?], [alignment?]

// Load and store
store %reg, %addr, alignment 
%reg = load %addr, alignment

ret %reg

*/
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
            if let TokenType::IntegerLiteral(val) = lit.token_type {
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

                    if let Node::InitDeclarator(init_declarator) = *init_declarator_list {
                        let reg = add_var_reg(scope_stack, init_declarator.declarator.name);
                        codegen_expr(scope_stack, reg, &mut block.instructions, init_declarator.initializer)?;
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

}
