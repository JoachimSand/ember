use std::fmt::Formatter;
use std::{iter::Peekable, fmt, error::Error};

use crate::parser::*;
use crate::lexer::*;

impl<'n> fmt::Display for Specifier<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        match self {
            Specifier::StorageClass(token) | Specifier::TypeQualifier(token) | Specifier::BasicType(token) => {
                write!(f, "\x1b[1;34m{:?}\x1b[0m", token)
            }

            Specifier::Struct{ is_union, name, ..} => {
                if *is_union {
                    write!(f, "union ")?;
                } else {
                    write!(f, "struct ")?;
                }

                if let Some(name) = name {
                    write!(f, "{:?}", name)?;
                } else {
                    write!(f, "anonymous ")?;
                }
                Ok(())
            }

            Specifier::Enum { name, .. } => {
                if let Some(name) = name {
                    write!(f, "enum {:?}", name)?;
                } else {
                    write!(f, "enum anonymous ")?;
                }
                Ok(())
            }
        }
    }
}

impl<'n> fmt::Display for StructDeclarationNode<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Struct Declaration")
    }
}

impl<'n> fmt::Display for Enumerator<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "{} {:?}", red!("Enumerator"), self.name)?;
        if let Some(val) = self.val {
            write!(f, ", val: {}", val)
        } else {
            write!(f, ", val unspecified")
        }
    }
}

impl<'n> fmt::Display for SpecifierList<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Specifiers")
    }
}

impl<'n> fmt::Display for DeclaratorNode<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        
        write!(f, "Declarator {}", self.name)?;

        if self.derived_types.len() > 0 {
            write!(f, ", ")?;
        }

        let mut iter = self.derived_types.iter().peekable();

        while let Some(node) = iter.next() {
            match *node {
                DerivedType::Pointer{is_const, is_volatile} =>{
                    if is_const {
                        write!(f, "c")?;
                    }

                    if is_volatile {
                        write!(f, "v")?;
                    }

                    write!(f, "* ")?;
                }
                DerivedType::FunctionParameterless => write!(f, "() ")?,
                DerivedType::Array { size }=> { 
                    if let Some(size) = size {
                        write!(f, "[{}] ", size)?;
                    } else {
                        write!(f, "[] ")?;
                    }
                }
                _ => write!(f, "Unimplemented")?,
            }
        }
        Ok(())
    }
}

impl<'n> fmt::Display for InitDeclaratorNode<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "{}", "Init Declarator")
    }
}

impl<'n> fmt::Display for IfNode<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "{}", red!("If Statement"))
    }
}

impl<'n> fmt::Display for Node<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::TranslationalUnit{..} => write!(f, "{}", yellow!("Translational Unit")),
            Node::FunctionDefinition{..} => write!(f, "{}", yellow!("Function Definition")),
            Node::CompoundStatement{..} => write!(f, "{}", ("Compound Statement")),
            Node::DeclarationList(_) => write!(f, "{}", cyan!("Declaration List ")),
            Node::StatementList(_) => write!(f, "{}", red!("Statement List")),
        
            Node::Declaration{..} => write!(f, "{}", cyan!("Declaration")),
            Node::InitDeclaratorList(_) => write!(f, "Init Declarator List"),
            Node::InitDeclarator(node) => write!(f, "{}", node),
            Node::InitializerList(_) => write!(f, "Initializer List"),
            
            Node::IfStatementList(_) => write!(f, "{}", red!("If Statement List")),
            Node::WhileStatement {..} => write!(f, "{}", red!("While Statement")),
            Node::DoWhileStatement{..} => write!(f, "{}", red!("Do While Statement")),
            Node::ForStatement{..} => write!(f, "{}", red!("For Statement")),
            
            Node::ArrayIndex{..} => write!(f, "Array Index"),
            Node::FunctionCall{..} => write!(f, "Function Call"),
            
            Node::Return { .. } => write!(f, "{}", red!("Return")),  
            Node::Infix{operator, ..} => write!(f, "I{:?}", operator),
            Node::Prefix(n) => write!(f, "Prefix {:?}", n.operator),
            Node::Postfix(n) => write!(f, "Postfix {:?}", n.operator),
            Node::Literal(t) => {
                
                write!(f, "\x1b[1;35m")?;
                match t {
                    Token::IntegerLiteral(val) => write!(f, "{}", val)?, 
                    Token::DoubleLiteral(val) => write!(f, "{}", val)?,
                    Token::FloatLiteral(val) => write!(f, "{}", val)?,
                    Token::StringLiteral(val) => write!(f, "{}", val)?,
                    _ => ()
                }
                write!(f, "\x1b[0m")
            }
            Node::Identifier{name} => write!(f, "\x1b[1;90m{}\x1b[0m", name),

            Node::Empty => write!(f, "{}", b_black!("Empty")),
            _ => write!(f, "Unknown Node"),

        }
    }
}

fn print_ast_prefix(prefix : String, is_last : bool) -> String{
    print!("{}", prefix);
    let mut new_prefix : String = prefix;
    if is_last {
        print!("└──");
        new_prefix.push_str("    ");
    } else {
        print!("├──");
        new_prefix.push_str("│  ");
    }
    return new_prefix;
}

pub fn print_ast_list<'n, T : fmt::Display, F: Fn(T, String, bool)>(node_list : impl IntoIterator<Item = T>, prefix : String, is_last : bool, print_child : bool, f : F)
{
    //let new_prefix = print_ast_prefix(prefix.clone(), is_last);
    let mut iter = node_list.into_iter().peekable();

    // If list is empty
    if iter.peek().is_none() {
        print_ast_prefix(prefix.clone(), is_last);
        println!("Empty");
        return;
    }

    while let Some(child) = iter.next(){
        if iter.peek().is_some() {
            if print_child {
                let child_prefix = print_ast_prefix(prefix.clone(), false);
                println!("{}", child);
                f(child, child_prefix, false);
            } else {
                f(child, prefix.clone(), false);
            }
        } else {
            if print_child {
                let child_prefix = print_ast_prefix(prefix.clone(), is_last);
                println!("{}", child);
                f(child, child_prefix, true);
            } else {
                f(child, prefix.clone(), true);
            }
        }
    }
}

pub fn print_ast_specifiers(specifiers_node : &SpecifierList, prefix : String, is_last : bool){
    let specifiers_prefix = print_ast_prefix(prefix.clone(), is_last);
    println!("{}", specifiers_node);
    
    let print_specifier = |child : &&Specifier, list_prefix : String, is_last : bool | {
        match child {
            Specifier::Struct { declaration_list, .. } => {
                //let specifier_prefix = print_ast_prefix(list_prefix.clone(), is_last)
                let print_struct_declaration = |declaration : &&StructDeclarationNode, prefix : String, _ : bool | {
                    print_ast_specifiers(declaration.specifier_qualifier_list, prefix.clone(), false);
                    print_ast_list(declaration.struct_declarator_list, prefix, true, true, |_, _, _| ());
                };

                if let Some(declaration_list) = declaration_list {
                    print_ast_list(declaration_list.iter(), list_prefix.clone(), is_last, true, print_struct_declaration);
                }
            }

            Specifier::Enum { enumerator_list, .. } => {
                if let Some(enumerator_list) = *enumerator_list {
                    print_ast_list(enumerator_list, list_prefix, is_last, true, |_, _, _| ());
                }
            }
            _ => return,
        }
    };
    print_ast_list(specifiers_node.specifiers.iter(), specifiers_prefix, true, true, print_specifier);
}

pub fn print_ast(start : &Node, prefix : String, is_last : bool) {
    let new_prefix = print_ast_prefix(prefix, is_last);
    println!("{}", start);
    
    match *start {
        
        Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
            print_ast_specifiers(declaration_specifiers, new_prefix.clone(), false);

            print_ast_prefix(new_prefix.clone(), false);
            println!("{declarator}");

            print_ast(compound_statement, new_prefix, true);
        }

        Node::CompoundStatement{declaration_list, statement_list} => {
            print_ast(declaration_list, new_prefix.clone(), false);
            print_ast(statement_list, new_prefix, true);
        }

        // quite possibly the coolest Rust syntax I have encountered so far
        Node::DeclarationList(list) | Node::StatementList(list) | Node::InitializerList(list) | Node::TranslationalUnit { external_declarations: list @ _ }=> {
            print_ast_list(list, new_prefix.clone(), true, false, |child, list_prefix, is_last| print_ast(child, list_prefix, is_last));
        }

        Node::InitDeclaratorList(list) => {
            print_ast_list(list, new_prefix.clone(), true, false, |child, list_prefix, is_last| print_ast(&Node::InitDeclarator(child), list_prefix, is_last));
        }

        Node::InitDeclarator(node) => {
            print_ast_prefix(new_prefix.clone(), false);
            println!("{}", node.declarator);
            print_ast(node.initializer, new_prefix, true);
        }

        Node::IfStatementList(list) => {

            let print_if_statement = |child : &&IfNode, child_prefix : String, is_last : bool | {
                print_ast(child.condition, child_prefix.clone(), false);
                print_ast(child.statement, child_prefix, true); 
            };
            print_ast_list(list, new_prefix.clone(), true, true, print_if_statement); 
        }

        Node::WhileStatement { condition, statement } => {
            print_ast(condition, new_prefix.clone(), false);
            print_ast(statement, new_prefix, true); 
        }

        Node::DoWhileStatement { statement, condition } => {
            print_ast(statement, new_prefix.clone(), false); 
            print_ast(condition, new_prefix, true);
        }

        Node::ForStatement { init_statement, condition, iter_statement } => {
           print_ast(init_statement, new_prefix.clone(), false);
           print_ast(condition, new_prefix.clone(), false); 
           print_ast(iter_statement, new_prefix, true); 
        }

        Node::FunctionCall { function, arguments } => {
            print_ast(function, new_prefix.clone(), false);
            print_ast_list(arguments, new_prefix.clone(), true, false, |child, list_prefix, is_last| print_ast(child, list_prefix, is_last));
        }

        Node::ArrayIndex { lvalue, index } => {
            print_ast(lvalue, new_prefix.clone(), false);
            print_ast(index, new_prefix, true);
        }

        Node::Return { expression } => {
            print_ast(expression, new_prefix, true);
        }

        Node::Empty | Node::Identifier{..} | Node::Literal(_) => {
            return;
        }
        Node::Infix{ left, right, .. } => {
            print_ast(left, new_prefix.clone(), false);
            print_ast(right, new_prefix, true);
        }
        Node::Prefix(node) => {
            print_ast(node.operand, new_prefix, true);
        }
        Node::Postfix(node) => {
            print_ast(node.operand, new_prefix, true);
        }

        Node::Declaration{ declaration_specifiers, init_declarator_list} => {
            print_ast_specifiers(declaration_specifiers, new_prefix.clone(), false);
            print_ast(init_declarator_list, new_prefix, true);
        }

        _ => {
            print!("Print AST node not implemented for {:?}", *start);
            return;
        }
    }
}

// TODO: This approach can be improved greatly.
// Might be worth using an external crate for this.
#[macro_export]
macro_rules! black {
    ( $l:literal) => {{
        colour!($l, "black")
    }}
}

#[macro_export]
macro_rules! red {
    ( $l:literal) => {{
        colour!($l, "red")
    }}
}

#[macro_export]
macro_rules! green {
    ( $l:literal) => {{
        colour!($l, "green")
    }}
}


#[macro_export]
macro_rules! yellow {
    ( $l:literal) => {{
        colour!($l, "yellow")
    }}
}

#[macro_export]
macro_rules! blue {
    ( $l:literal) => {{
        colour!($l, "blue")
    }}
}

#[macro_export]
macro_rules! magenta {
    ( $l:literal) => {{
        colour!($l, "magenta")
    }}
}

#[macro_export]
macro_rules! cyan {
    ( $l:literal) => {{
        colour!($l, "cyan")
    }}
}

#[macro_export]
macro_rules! white {
    ( $l:literal) => {{
        colour!($l, "white")
    }}
}

#[macro_export]
macro_rules! default {
    ( $l:literal) => {{
        colour!($l, "default")
    }}
}

#[macro_export]
macro_rules! b_black {
    ( $l:literal) => {{
        colour!($l, "b_black")
    }}
}

#[macro_export]
macro_rules! b_red {
    ( $l:literal) => {{
        colour!($l, "b_red")
    }}
}

#[macro_export]
macro_rules! b_green {
    ( $l:literal) => {{
        colour!($l, "b_green")
    }}
}

#[macro_export]
macro_rules! b_yellow {
    ( $l:literal) => {{
        colour!($l, "b_yellow")
    }}
}

#[macro_export]
macro_rules! b_blue {
    ( $l:literal) => {{
        colour!($l, "b_blue")
    }}
}

#[macro_export]
macro_rules! b_magenta {
    ( $l:literal) => {{
        colour!($l, "b_magenta")
    }}
}

#[macro_export]
macro_rules! b_cyan {
    ( $l:literal) => {{
        colour!($l, "b_cyan")
    }}
}

#[macro_export]
macro_rules! d_white {
    ( $l:literal) => {{
        colour!($l, "d_white")
    }}
}

#[macro_export]
macro_rules! colour {
    ( $l:expr, $c:literal) => {{
        let ret : &str = match $c {
            "black" =>      concat!("\x1b[1;30m", $l, "\x1b[0m"),
            "red" =>        concat!("\x1b[1;31m", $l, "\x1b[0m"),
            "green" =>      concat!("\x1b[1;32m", $l, "\x1b[0m"),
            "yellow" =>     concat!("\x1b[1;33m", $l, "\x1b[0m"),
            "blue" =>       concat!("\x1b[1;34m", $l, "\x1b[0m"),
            "magenta" =>    concat!("\x1b[1;35m", $l, "\x1b[0m"),
            "cyan" =>       concat!("\x1b[1;36m", $l, "\x1b[0m"),
            "white" =>      concat!("\x1b[1;37m", $l, "\x1b[0m"),
            "default" =>    concat!("\x1b[1;39m", $l, "\x1b[0m"),
            
            // bright colours
            "b_black" =>    concat!("\x1b[1;90m", $l, "\x1b[0m"),
            "b_red" =>      concat!("\x1b[1;91m", $l, "\x1b[0m"),
            "b_green" =>    concat!("\x1b[1;92m", $l, "\x1b[0m"),
            "b_yellow" =>   concat!("\x1b[1;93m", $l, "\x1b[0m"),
            "b_blue" =>     concat!("\x1b[1;94m", $l, "\x1b[0m"),
            "b_magenta" =>  concat!("\x1b[1;95m", $l, "\x1b[0m"),
            "b_cyan" =>     concat!("\x1b[1;96m", $l, "\x1b[0m"),
            "b_white" =>    concat!("\x1b[1;97m", $l, "\x1b[0m"),
            
            // dim colours
            "d_white" =>    concat!("\x1b[1;37;1m", $l, "\x1b[0m"),
            _ =>            concat!("\x1b[0;0m", $l, "\x1b[0m"),
        };
        ret
    }}
}
