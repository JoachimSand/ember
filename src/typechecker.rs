use std::collections::HashMap;
use std::fmt;

use crate::compile::CompilationError;
use crate::lexer::*;
use crate::parser::*;

/*
// See https://blog.robertelder.org/building-a-c-compiler-type-system-a-canonical-type-representation/ 
pub struct Type<'t>{
    // e.g. int, long, const volatile etc.
    specifiers : &'t [Token<'t>],
    
    // e.g. pointer deref, array index etc.
    derived_types : &'t [DerivedType<'t>],
}

// Type used locally when typechecking expressions
pub struct ExprType<'t> {
    specifiers : Vec<Token<'t>>,
    derived_types : Vec<DerivedType<'t>>,
}

impl<'t> fmt::Display for Type<'t>{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "type: {:?}, {:?}", self.specifiers, self.derived_types)
    }
}

type Scope<'s, 't> = HashMap<&'s str, Type<'t>>;
pub type Scopes<'s, 't> = Vec<Scope<'s, 't>>;

fn push_new_scope(scopes : &mut Scopes){
    let scope = Scope::new();

    scopes.push(scope);
}

fn pop_scope(scopes : &mut Scopes){
    scopes.pop();
}

// Note: By variable we refer to functions as well
fn push_variable<'s, 't : 's>(scopes : &mut Scopes<'s, 't>, var_name : &'t str, var_type : Type<'t>) -> Result<(), CompilationError<'t>>
{
    let cur_scope = scopes.last_mut().ok_or(CompilationError::NoScopes)?;

    println!("Adding {var_name} with type {var_type} to a scope");
    if let Some(_) = cur_scope.insert(var_name, var_type) {
        return Err(CompilationError::Redefinition(var_name));
    } 
    return Ok(());
}

fn type_check_expr<'s, 'n : 's>(scopes : &Scopes<'s, 'n>, expr : &'n Node<'n>) -> Result<ExprType<'n>, CompilationError<'n>>{
    match *expr {
        Node::Literal(literal) => {
            match literal {
                Token::IntegerLiteral(_) => {
                    let specifiers = vec![Token::Int];
                    let expr_type = ExprType{specifiers : vec![Token::Int], derived_types : Vec::<DerivedType>::new()};
                    return Ok(expr_type);
                }

                _ => return Err(CompilationError::InvalidASTStructure),
            }
        }

        Node::Infix { operator, left, right } => {
            let left_type = type_check_expr(scopes, left)?;
            let right_type = type_check_expr(scopes, right)?;

            if left_type.derived_types.is_empty() && right_type.derived_types.is_empty() {
                // Both left and right are arithmetic, perform type promotion

                // TODO: Add suport for more basic types e.g. short
                Err(CompilationError::NotImplemented)

            } else {
                return Err(CompilationError::NotImplemented);
            }
            
        }

        _ => {
            return Err(CompilationError::InvalidASTStructure);
        }
    }
}

fn type_check_compound_stmt<'s, 'n : 's>(scopes : &mut Scopes<'s, 'n>, compound_statement : &'n Node<'n>) -> Result<(), CompilationError<'n>>{
    push_new_scope(scopes);

    if let Node::CompoundStatement { declaration_list, statement_list } = compound_statement {
        if let Node::DeclarationList(list) = declaration_list {
            for declaration in *list {
                if let Node::Declaration { declaration_specifiers, init_declarator_list } = *declaration {
                    if let Node::InitDeclarator{ declarator, initializer} = *init_declarator_list {
                        let var_type = Type{ specifiers : declaration_specifiers.specifiers, derived_types : declarator.derived_types};
                        push_variable(scopes, declarator.name, var_type)?;
                        
                        // TODO: Typecheck initializer
                    } else {
                        return Err(CompilationError::NotImplemented);
                    }
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
                    let func_type : Type = Type{specifiers : declaration_specifiers.specifiers, derived_types : declarator.derived_types};
                    push_variable(scopes, declarator.name, func_type)?;
                    
                    type_check_compound_stmt(scopes, compound_statement)?;
                }

                Node::Declaration { declaration_specifiers, init_declarator_list } => {
                    
                    // Just a a single declarator
                    if let Node::InitDeclarator { declarator, initializer } = init_declarator_list {
                        let declaration_type = Type{ specifiers : declaration_specifiers.specifiers, derived_types : declarator.derived_types};
                        push_variable(scopes, declarator.name, declaration_type)?;
                    } else {
                        // TODO: Implement initializer lists
                        return Err(CompilationError::NotImplemented);
                    }
                }
                _ => return Err(CompilationError::InvalidASTStructure),
            } 
        }

        Ok(())
    } else {
        return Err(CompilationError::InvalidASTStructure);
    }
}*/
