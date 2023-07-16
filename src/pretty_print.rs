use std::fmt;

use crate::parser::*;
use crate::lexer::*;

impl<'n> fmt::Display for Specifier<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        match self {
            Specifier::StorageClass(token) | Specifier::TypeQualifier(token) | Specifier::BasicType(token) => {
                write!(f, "\x1b[1;34m{:?}\x1b[0m", token.token_type)
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
        
        write!(f, "{} {}", green!("Declarator"), self.name)?;

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

                    write!(f, "{}", green!("* "))?;
                }
                DerivedType::FunctionParameterless => write!(f, "{}", green!("() "))?,
                DerivedType::Array { size }=> { 
                    if let Some(size) = size {
                        write!(f, "{}{}{} ", green!("["), size, green!("]"))?;
                    } else {
                        write!(f, "{}", green!("[] "))?;
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
            Node::Infix{operator, ..} => write!(f, "I{:?}", operator.token_type),
            Node::Prefix(n) => write!(f, "Prefix {:?}", n.operator.token_type),
            Node::Postfix(n) => write!(f, "Postfix {:?}", n.operator.token_type),
            Node::Literal(t) => {
                
                write!(f, "\x1b[1;35m")?;
                match t.token_type {
                    TokenType::IntegerLiteral(val) => write!(f, "{}", val)?, 
                    TokenType::DoubleLiteral(val) => write!(f, "{}", val)?,
                    TokenType::FloatLiteral(val) => write!(f, "{}", val)?,
                    TokenType::StringLiteral(val) => write!(f, "{}", val)?,
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

    let print_child = |child : &Node| {
        print_ast(child, new_prefix.clone(), false);
    };

    let print_last_child = |child : &Node| {
        print_ast(child, new_prefix.clone(), true);
    };
    
    match *start {
        
        Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
            print_ast_specifiers(declaration_specifiers, new_prefix.clone(), false);

            print_ast_prefix(new_prefix.clone(), false);
            println!("{declarator}");

            print_last_child(compound_statement);
        }

        Node::CompoundStatement{declaration_list, statement_list} => {
            print_child(declaration_list);
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
            print_last_child(node.initializer);
        }

        Node::IfStatementList(list) => {

            let print_if_statement = |child : &&IfNode, child_prefix : String, _ : bool | {
                print_ast(child.condition, child_prefix.clone(), false);
                print_ast(child.statement, child_prefix, true); 
            };
            print_ast_list(list, new_prefix.clone(), true, true, print_if_statement); 
        }

        Node::WhileStatement { condition, statement } => {
            print_child(condition);
            print_last_child(statement); 
        }

        Node::DoWhileStatement { statement, condition } => {
            print_child(statement); 
            print_last_child(condition);
        }

        Node::ForStatement { init_statement, condition, iter_statement, statement } => {
            print_child(init_statement);
            print_child(condition); 
            print_child(iter_statement);
            print_last_child(statement);
        }

        Node::FunctionCall { function, arguments } => {
            print_ast(function, new_prefix.clone(), false);
            print_ast_list(arguments, new_prefix.clone(), true, false, |child, list_prefix, is_last| print_ast(child, list_prefix, is_last));
        }

        Node::ArrayIndex { lvalue, index } => {
            print_child(lvalue);
            print_last_child(index);
        }

        Node::Return { expression } => {
            print_ast(expression, new_prefix, true);
        }

        Node::Empty | Node::Identifier{..} | Node::Literal(_) => {
            return;
        }
        Node::Infix{ left, right, .. } => {
            print_child(left);
            print_last_child(right);
        }
        Node::Prefix(node) => {
            print_last_child(node.operand);
        }
        Node::Postfix(node) => {
            print_last_child(node.operand);
        }

        Node::Declaration{ declaration_specifiers, init_declarator_list} => {
            print_ast_specifiers(declaration_specifiers, new_prefix.clone(), false);
            print_last_child(init_declarator_list);
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
        colour!($l, "1;30m")
    }}
}

#[macro_export]
macro_rules! red {
    ( $l:literal) => {{
        colour!($l, "1;31m")
    }}
}

#[macro_export]
macro_rules! green {
    ( $l:literal) => {{
        colour!($l, "1;32m")
    }}
}


#[macro_export]
macro_rules! yellow {
    ( $l:literal) => {{
        colour!($l, "1;33m")
    }}
}

#[macro_export]
macro_rules! blue {
    ( $l:literal) => {{
        colour!($l, "1;34m")
    }}
}

#[macro_export]
macro_rules! magenta {
    ( $l:literal) => {{
        colour!($l, "1;35m")
    }}
}

#[macro_export]
macro_rules! cyan {
    ( $l:literal) => {{
        colour!($l, "1;36m")
    }}
}

#[macro_export]
macro_rules! white {
    ( $l:literal) => {{
        colour!($l, "1;37m")
    }}
}

#[macro_export]
macro_rules! default {
    ( $l:literal) => {{
        colour!($l, "1;39m")
    }}
}

#[macro_export]
macro_rules! b_black {
    ( $l:literal) => {{
        colour!($l, "1;90m")
    }}
}

#[macro_export]
macro_rules! b_red {
    ( $l:literal) => {{
        colour!($l, "1;91m")
    }}
}

#[macro_export]
macro_rules! b_green {
    ( $l:literal) => {{
        colour!($l, "1;92m")
    }}
}

#[macro_export]
macro_rules! b_yellow {
    ( $l:literal) => {{
        colour!($l, "1;93m")
    }}
}

#[macro_export]
macro_rules! b_blue {
    ( $l:literal) => {{
        colour!($l, "1;94m")
    }}
}

#[macro_export]
macro_rules! b_magenta {
    ( $l:literal) => {{
        colour!($l, "1;95m")
    }}
}

#[macro_export]
macro_rules! b_cyan {
    ( $l:literal) => {{
        colour!($l, "1;96m")
    }}
}

#[macro_export]
macro_rules! d_white {
    ( $l:literal) => {{
        colour!($l, "1;37;1m")
    }}
}

#[macro_export]
macro_rules! colour {
    ( $l:expr, $format:literal) => {{
        concat!("\x1b[", $format, $l, "\x1b[0m")
    }}
}

#[allow(unused_imports)]
pub(crate) use {
    black, red, green, yellow, blue, magenta, cyan, white, default, 
    b_black, b_red, b_green, b_yellow, b_blue, b_magenta, b_cyan,
    d_white, 
    colour
};
