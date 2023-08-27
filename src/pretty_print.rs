use std::println;

use crate::parser::*;
use crate::lexer::*;

pub fn print_declarator(declarator : &DeclaratorNode, prefix : String, is_last : bool){
    let new_prefix = print_ast_prefix(prefix.clone(), is_last);

    if let Some(name) = declarator.name {
        println!("{} {}", green!("Declarator"), name);
    } else {
        println!("{}", green!("Abstract Declarator"));
    }

    if declarator.derived_types.is_empty() {
        return;
    }
    
    let mut iter = declarator.derived_types.iter().peekable();

    while let Some(node) = iter.next() {
        let derived_type_prefix = if iter.peek().is_some() {
            print_ast_prefix(new_prefix.clone(), false)
        } else {            
            print_ast_prefix(new_prefix.clone(), true)
        };

        match *node {
            DerivedType::Pointer{is_const, is_volatile} =>{
                if is_const {
                    print!("c");
                }

                if is_volatile {
                    print!("v");
                }

                println!("{}", green!("* "));
            }
            DerivedType::FunctionParameterless => println!("{}", green!("() ")),
            DerivedType::Function { parameter_type_list } => {
                println!("{}", green!("() "));

                for (p, param_declaration) in parameter_type_list.param_declarations.iter().enumerate() {

                    let param_prefix = print_ast_prefix(derived_type_prefix.clone(), p == parameter_type_list.param_declarations.len() - 1);
                    println!("Parameter Declaration");
                    
                    if let Some(declarator) = param_declaration.declarator {
                        print_ast_specifiers(param_declaration.declaration_specifiers, param_prefix.clone(), false);
                        print_declarator(declarator, param_prefix.clone(), true);
                    } else {
                        print_ast_specifiers(param_declaration.declaration_specifiers, param_prefix.clone(), true);
                    }
                    
                    
                }
                //parameter_type_list.param_declarations[0].declaration_specifiers
            }
            DerivedType::Array { size }=> { 
                if let Some(size) = size {
                    println!("{}{}{} ", green!("["), size, green!("]"));
                } else {
                    println!("{}", green!("[] "));
                }
            }
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

pub fn print_ast_list<'e, T : 'e, F: Fn(&T, String, bool)>(node_list : &[&T], prefix : String, is_last : bool, child_recurs : F)
{
    //let new_prefix = print_ast_prefix(prefix.clone(), is_last);
    //let mut iter = node_list.into_iter().peekable();

    // If list is empty
    if node_list.is_empty() {
        print_ast_prefix(prefix.clone(), is_last);
        println!("Empty");
        return;
    }

    for (index, child) in node_list.iter().enumerate() {
        if index == node_list.len() - 1 {
            child_recurs(*child, prefix.clone(), true);
        } else {
            child_recurs(*child, prefix.clone(), false);
        }
    }
}

pub fn print_ast_specifiers(specifiers_node : &Specifiers, prefix : String, is_last : bool){
    let specifiers_prefix = print_ast_prefix(prefix.clone(), is_last);

    if specifiers_node.is_typedef {
        print!("typedef ");
    }
    if specifiers_node.is_const {
        print!("const ");
    }
    if specifiers_node.is_volatile {
        print!("volatile ");
    }
    if specifiers_node.is_extern {
        print!("extern ");
    }
    if specifiers_node.is_static {
        print!("static ");
    }
    
    match specifiers_node.type_specifier {
        TypeSpecifier::UnsignedChar => println!("unsigned char"),
        TypeSpecifier::SignedChar => println!("signed char"),
        TypeSpecifier::Short => println!("short"),
        TypeSpecifier::UnsignedShort => println!("unsigned short"),
        TypeSpecifier::Int => println!("int"),
        TypeSpecifier::UnsignedInt => println!("unsigned int"),
        TypeSpecifier::Long => println!("long"),
        TypeSpecifier::UnsignedLong => println!("unsigned long"),
        TypeSpecifier::LongLong => println!("long long"),
        TypeSpecifier::UnsignedLongLong => println!("unsigned long long"),
        TypeSpecifier::Float => println!("float"),
        TypeSpecifier::Double => println!("double"),
        TypeSpecifier::LongDouble => println!("long double"),
        TypeSpecifier::Void => println!("void"),
        TypeSpecifier::TypeAlias(ta) => println!("\x1b[1;35m{}\x1b[0m", ta.name),
        TypeSpecifier::None => println!("no type specifier"),
        TypeSpecifier::Struct { declaration_list, is_union, name } => {

            if is_union {
                print!("union ")
            } else {
                print!("struct ")
            }

            if let Some(name) = name {
                println!("{}", name)
            } else {
                println!("anonymous ")
            }
            //let specifier_prefix = print_ast_prefix(list_prefix.clone(), is_last)
            let print_struct_declaration = |declaration : &StructDeclarationNode, prefix : String, is_last : bool | {
                print_ast_specifiers(declaration.specifiers, prefix.clone(), is_last);
                print_ast_list(declaration.struct_declarator_list, prefix, true, |_, _, _| ());
            };

            if let Some(declaration_list) = declaration_list {
                print_ast_list(declaration_list, specifiers_prefix.clone(), is_last, print_struct_declaration);
            }
        }

        TypeSpecifier::Enum { enumerator_list, name } => {

            if let Some(name) = name {
                println!("enum {:?}", name)
            } else {
                println!("enum anonymous")
            }
            
            if let Some(enumerator_list) = enumerator_list {
                let print_enumerator = |enumerator: &Enumerator, prefix : String, is_last : bool | {
                    print_ast_prefix(prefix.clone(), is_last);
                    if let Some(val) = enumerator.val {
                        println!("\x1b[1;90m{}\x1b[0m = \x1b[1;35m{}\x1b[0m", enumerator.name, val);
                    } else {                           
                        println!("\x1b[1;90m{}\x1b[0m, no val.", enumerator.name);
                    }
                };

                print_ast_list(enumerator_list, specifiers_prefix, is_last, print_enumerator);
            }
        }
    }
}

pub fn print_ast(start : &Node, prefix : String, is_last : bool) {
    let new_prefix = print_ast_prefix(prefix, is_last);
    //println!("{}", start);

    let print_child = |child : &Node| {
        print_ast(child, new_prefix.clone(), false);
    };

    let print_last_child = |child : &Node| {
        print_ast(child, new_prefix.clone(), true);
    };
    
    match *start {
        
        Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
            
            println!("{}", yellow!("Function Definition"));
            print_ast_specifiers(declaration_specifiers, new_prefix.clone(), false);

            print_declarator(declarator, new_prefix.clone(), false);           
            print_last_child(compound_statement);
        }

        Node::CompoundStatement{declaration_list, statement_list} => {
            println!("Compound Statement");
            print_child(declaration_list);
            print_ast(statement_list, new_prefix, true);
        }

        Node::DeclarationList(list) | Node::StatementList(list) | Node::InitializerList(list) | Node::TranslationalUnit { external_declarations: list @ _ }=> {
            match *start {
                Node::DeclarationList(_) => println!("{}", cyan!("Declaration List ")),
                Node::StatementList(_) => println!("{}", red!("Statement List")),
                Node::InitializerList(_) => println!("Initializer List"),
                Node::TranslationalUnit{..} => println!("{}", yellow!("Translational Unit")),
                _ => panic!("Unreachable"),
            }
            print_ast_list(list, new_prefix.clone(), true, print_ast);
        }

        Node::InitDeclaratorList(list) => {
            println!("Init Declarator List");
            let print_init_declarator = |init_declarator: &InitDeclaratorNode, prefix: String, is_last : bool| {
                
                let init_decl_prefix = print_ast_prefix(prefix.clone(), is_last);
                println!("Init Declarator");

                print_declarator(init_declarator.declarator, init_decl_prefix.clone(), false);
                print_ast(init_declarator.initializer, init_decl_prefix.clone(), true);
            };
            print_ast_list(list, new_prefix.clone(), true, print_init_declarator);
        }

        Node::IfStatementList(list) => {
            println!("{}", red!("If Statement List"));
            let print_if_statement = |child : &IfNode, child_prefix : String, _ : bool | {
                print_ast(child.condition, child_prefix.clone(), false);
                print_ast(child.statement, child_prefix, true); 
            };
            print_ast_list(list, new_prefix.clone(), true, print_if_statement); 
        }

        Node::WhileStatement { condition, statement } => {
            println!("{}", red!("While Statement"));
            print_child(condition);
            print_last_child(statement); 
        }

        Node::DoWhileStatement { statement, condition } => {
            println!("{}", red!("Do While Statement"));
            print_child(statement); 
            print_last_child(condition);
        }

        Node::ForStatement { init_statement, condition, iter_statement, statement } => {
            println!("{}", red!("For Statement"));
            print_child(init_statement);
            print_child(condition); 
            print_child(iter_statement);
            print_last_child(statement);
        }

        Node::FunctionCall { function, arguments } => {
            println!("Function Call");
            print_ast(function, new_prefix.clone(), false);
            print_ast_list(arguments, new_prefix.clone(), true, print_ast);
        }

        Node::ArrayIndex { lvalue, index } => {
            println!("Array Index");
            print_child(lvalue);
            print_last_child(index);
        }

        Node::Return { expression } => {
            println!("{}", red!("Return"));
            print_ast(expression, new_prefix, true);
        }

        Node::Empty => {
            println!("{}", b_black!("Empty"));             
        }

        Node::Identifier{ name } => {
            println!("\x1b[1;90m{}\x1b[0m", name); 
        } 
        
        Node::Literal(lit) => {
            print!("\x1b[1;35m");
            match lit.token_type {
                TokenType::IntegerLiteral(val) => print!("{}", val), 
                TokenType::DoubleLiteral(val) => print!("{}", val),
                TokenType::FloatLiteral(val) => print!("{}", val),
                TokenType::StringLiteral(val) => print!("{}", val),
                _ => ()
            }
            println!("\x1b[0m");
        }

        Node::Infix{ operator, left, right } => {
            println!("I{:?}", operator.token_type);
            print_child(left);
            print_last_child(right);
        }

        Node::Prefix(node) => {
            println!("Prefix {:?}", node.operator.token_type);
            print_last_child(node.operand);
        }

        Node::Postfix(node) => {
            println!("Postfix {:?}", node.operator.token_type);
            print_last_child(node.operand);
        }

        Node::Declaration{ declaration_specifiers, init_declarator_list} => {
            println!("{}", cyan!("Declaration"));
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
