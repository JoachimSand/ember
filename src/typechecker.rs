use std::collections::HashMap;
use std::fmt;

use crate::compile::CompilationError;
use crate::ir::{IRType, Register};
use crate::lexer::*;
use crate::parser::*;

// See https://blog.robertelder.org/building-a-c-compiler-type-system-a-canonical-type-representation/
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Type<'t> {
    // e.g. int, long, const volatile etc.
    pub specifiers: &'t Specifiers<'t>,

    // e.g. pointer deref, array index etc.
    pub derived_types: &'t [DerivedType<'t>],
}

// Type used locally when typechecking expressions
#[derive(Default)]
pub struct ExprType<'t> {
    pub specifiers: Specifiers<'t>,
    pub derived_types: Vec<DerivedType<'t>>,
}

#[derive(Clone, Copy)]
pub struct VariableInfo<'t> {
    pub c_type: Type<'t>,
    pub ir_type: IRType,
    pub reg: Register<'t>,
}

#[derive(Debug, PartialEq)]
pub struct TypeAlias<'t> {
    pub name: &'t str,
    pub alias: Type<'t>,
}

impl<'t> fmt::Display for Type<'t> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}", self.specifiers, self.derived_types)
    }
}

type Scope<'s, 't> = HashMap<&'s str, VariableInfo<'t>>;
pub type Scopes<'s, 't> = Vec<Scope<'s, 't>>;

pub fn push_new_scope(scopes: &mut Scopes) {
    let scope = Scope::new();
    scopes.push(scope);
}

pub fn pop_scope(scopes: &mut Scopes) {
    scopes.pop();
}

// Note: By variable we refer to functions as well
pub fn push_reg_type<'r, 's: 'r, 't: 's>(
    scopes: &'r mut Scopes<'s, 't>,
    var_name: &'t str,
    var_type: VariableInfo<'t>,
) -> Result<(), CompilationError<'t>> {
    println!("Adding {var_name} to scope {}", scopes.len());

    let cur_scope = scopes.last_mut().ok_or(CompilationError::NoScopes)?;

    //println!("Adding {var_name} with type {var_type} to a scope");
    if let Some(_) = cur_scope.insert(var_name, var_type) {
        return Err(CompilationError::Redefinition(var_name));
    }
    return Ok(());
}

pub fn get_var_info<'s, 't>(scopes: &'s Scopes<'s, 't>, name: &'t str) -> Option<VariableInfo<'t>> {
    for scope in scopes {
        if let Some(var_info) = scope.get(name) {
            return Some(*var_info);
        }
    }
    return None;
}

/*

fn type_check_compound_stmt<'s, 'n : 's>(scopes : &mut Scopes<'s, 'n>, compound_statement : &'n Node<'n>) -> Result<(), CompilationError<'n>>{
    push_new_scope(scopes);

    if let Node::CompoundStatement { declaration_list, statement_list } = compound_statement {
        if let Node::DeclarationList(decl_list) = declaration_list {

            // Run through every declaration, push it to the scope
            for declaration in *decl_list {
                match declaration {
                    Node::Declaration { declaration_specifiers, init_declarator_list : Node::InitDeclaratorList(init_list) } => {
                        for init_decl in *init_list {
                            let var_type = Type{ specifiers : declaration_specifiers, derived_types : init_decl.declarator.derived_types};
                            push_variable(scopes, init_decl.declarator.name.unwrap(), var_type)?;
                        }
                    }
                    _ => return Err(CompilationError::InvalidASTStructure)
                }
            }
        }

        if let Node::StatementList(statmt_list) = statement_list {
            for statmt in *statmt_list {
                match statmt {
                    Node::CompoundStatement { .. } => {
                        type_check_compound_stmt(scopes, statmt)?;
                    }

                    Node::IfStatementList(if_list) => {
                        for if_stmt in *if_list {
                            //if_stmt.condition

                        }
                    }

                    _ => return Err(CompilationError::InvalidASTStructure)
                }
            }
        }
    }
    pop_scope(scopes);
    Ok(())
}


pub fn type_check_start<'n>(translation_unit : &'n Node<'n>) -> Result<(), CompilationError>{

    let scopes = &mut Scopes::new();
    // Global Scope
    push_new_scope(scopes);

    if let Node::TranslationalUnit { external_declarations } = *translation_unit {
        for external_declaration in external_declarations {
            match *external_declaration {

                Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
                    let func_type : Type = Type{specifiers : declaration_specifiers, derived_types : declarator.derived_types};
                    push_variable(scopes, declarator.name.unwrap(), func_type)?;
                    type_check_compound_stmt(scopes, compound_statement)?;
                }

                Node::Declaration { declaration_specifiers, init_declarator_list : Node::InitDeclaratorList(list) } => {
                    if declaration_specifiers.is_typedef {
                        continue;
                    }

                    for init_declarator in *list {
                        let declarator = init_declarator.declarator;
                        let declaration_type = Type{ specifiers : declaration_specifiers, derived_types : declarator.derived_types};
                        push_variable(scopes, declarator.name.unwrap(), declaration_type)?;
                    }
                }
                _ => return Err(CompilationError::InvalidASTStructure),
            }
        }

        Ok(())
    } else {
        return Err(CompilationError::InvalidASTStructure);
    }
}
*/
