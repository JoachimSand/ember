use std::{iter::Peekable, fmt, error::Error, ops::{Deref}};
use crate::lexer::*;

#[derive(Debug)]
pub enum ParserError {
    NotImplemented,
    UnexpectedToken(Token),
    EarlyLexerTermination,
    UnknownPrecedence(Token),
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Parser Error: {}", self.to_string())
    }
}

type NodePtr = Box<Node>;

#[derive(Clone, Debug)]
pub struct BinaryNode {
    operator : Token,
    left : NodePtr,
    right : NodePtr,
}

#[derive(Clone, Debug)]
pub struct UnaryNode {
    operator : Token,
    operand : NodePtr,
}

#[derive(Clone, Debug)]
pub struct ArrayIndexNode {
    lvalue : NodePtr,
    index : NodePtr,
}

#[derive(Clone, Debug)]
pub struct DeclarationNode {
    declaration_specifiers : NodePtr,
    init_declarator_list : NodePtr,
}

#[derive(Clone, Debug)]
pub struct DeclaratorNode {
    // TODO: Add pointer derived type
    // int (*p[4]) (int x, int y); 
    // "identifier p is an array of 4 of pointers to ..." Layer 1
    // " ... a function taking two ints returning int" Layer 2

    // What the declarator is "modifying". In most cases this would be an 
    // identifier, but could also be another declarator node!
    base : NodePtr,

    // Pointer to postfix derived type node (either an array or function declarator)
    // which specifies how this declarator node is modifying the base.
    //
    postfix_derived_type : NodePtr,
}


#[derive(Clone, Debug)]
pub struct InitDeclaratorNode {
    declarator : NodePtr,
    initializer : NodePtr,
}


#[derive(Clone, Debug)]
pub enum Node { 
    Infix(BinaryNode),
    Prefix(UnaryNode),
    Postfix(UnaryNode),

    ArrayIndex(ArrayIndexNode),
    Terminal(Token),
 
    Declaration(DeclarationNode),
    DeclarationSpecifiers(Vec<NodePtr>),
    TypeQualifier(Token),
    TypeSpecifier(Token),
    _StorageSpecifier(Token),

    InitDeclarator(InitDeclaratorNode),
    ArrayDeclarator(DeclaratorNode),
    FunctionDeclarator(DeclaratorNode),

    Empty,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Infix(n) => write!(f, "I{:?}", n.operator),
            Node::Prefix(n) => write!(f, "Prefix {:?}", n.operator),
            Node::Postfix(n) => write!(f, "Postfix {:?}", n.operator),
            Node::ArrayIndex(_) => write!(f, "Array Index"),
            Node::Terminal(t) => write!(f, "{:?}", t),

            Node::Declaration(_) => write!(f, "Declaration"),
            Node::DeclarationSpecifiers(_) => write!(f, "Declaration Specifiers"),
            Node::TypeSpecifier(t) => write!(f, "{:?}", t),
            Node::TypeQualifier(t) => write!(f, "{:?}", t),
            Node::_StorageSpecifier(t) => write!(f, "{:?}", t),
            Node::Empty => write!(f, "Empty"),

            Node::InitDeclarator(_) => write!(f, "Init Declarator"),
            Node::FunctionDeclarator(_) => write!(f, "Func Declarator"),
            _ => write!(f, "Unknown Node"),

        }
    }
}


struct OperatorInfo {
    precedence : u8,
    left_associative : bool,
}

pub fn print_ast(start : NodePtr, prefix : String, is_last : bool) {

    print!("{}", prefix);


    let mut new_prefix : String = prefix;
    if is_last {
        print!("└──");
        new_prefix.push_str("    ");
    } else {
        print!("├──");
        new_prefix.push_str("│  ");
    }

    println!("{}", start.deref());
    
    match *start {
        Node::Infix(node) => {
            print_ast(node.left, new_prefix.clone(), false);
            print_ast(node.right, new_prefix, true);
        }
        Node::Prefix(node) => {
            print_ast(node.operand, new_prefix, true);
        }
        Node::Postfix(node) => {
            print_ast(node.operand, new_prefix, true);
        }
        Node::ArrayIndex(node) => {
            print_ast(node.lvalue, new_prefix.clone(), false);
            print_ast(node.index, new_prefix, true);
        }
        Node::DeclarationSpecifiers(specifier_list) => {

            for n in 0..(specifier_list.len() - 1) {
                print_ast(specifier_list[n].clone(), new_prefix.clone(), false);
            }
            print_ast(specifier_list[specifier_list.len() - 1].clone(), new_prefix, true);
        }
        Node::FunctionDeclarator(node) => {
            print_ast(node.base, new_prefix.clone(), false);
            print_ast(node.postfix_derived_type, new_prefix, true);
        }

        Node::Declaration(node) => {
            print_ast(node.declaration_specifiers, new_prefix.clone(), false);
            print_ast(node.init_declarator_list, new_prefix, true);
        }

        Node::InitDeclarator(node) => {
            print_ast(node.declarator, new_prefix.clone(), false);
            print_ast(node.initializer, new_prefix, true);
        }
        _ => return,
    }
}


// Lookup table for binary operators
// https://en.cppreference.com/w/c/language/operator_precedence
fn operator_lookup(token : &Token) -> Result<OperatorInfo, ParserError> {
    match token {
        Token::Asterisk | Token::Div | Token::Mod => {
            return Result::Ok(OperatorInfo{precedence : 200, left_associative : true});
        }

        Token::Plus | Token::Minus => {
            return Result::Ok(OperatorInfo{precedence : 190, left_associative : true});
        }
        
        Token::LeftShift | Token::RightShift => {
            return Result::Ok(OperatorInfo{precedence : 180, left_associative : true});
        }

        Token::LessThan | Token::LessThanOrEq | Token::GreaterThan | Token::GreaterThanOrEq => {
            return Result::Ok(OperatorInfo{precedence : 170, left_associative : true});
        }

        Token::Equals | Token::NotEquals => {
            return Result::Ok(OperatorInfo{precedence : 160, left_associative : true});
        }

        Token::Ampersand => { // bitwise AND
            return Result::Ok(OperatorInfo{precedence : 150, left_associative : true});
        }

        Token::XOR => { // bitwise XOR
            return Result::Ok(OperatorInfo{precedence : 140, left_associative : true});
        }

        Token::OR => { // bitwise OR
            return Result::Ok(OperatorInfo{precedence : 130, left_associative : true});
        }

        Token::ANDLogical => { 
            return Result::Ok(OperatorInfo{precedence : 120, left_associative : true});
        }

        Token::ORLogical => { 
            return Result::Ok(OperatorInfo{precedence : 110, left_associative : true});
        }

        // TODO: Ternary 
        
        Token::Assign | Token::PlusAssign | Token::MinusAssign |
        Token::MultAssign | Token::DivAssign | Token::ModAssign |
        Token::LeftShiftAssign | Token::RightShiftAssign |
        Token::ANDAssign | Token::XORAssign | Token::ORAssign=> {
            return Result::Ok(OperatorInfo{precedence : 90, left_associative : false});
        }

        _ => return Result::Err(ParserError::UnknownPrecedence(token.clone())),
    }
}


fn expect_token(lexer : &mut Peekable<Lexer>, expect : Token) -> Result<(), ParserError> {
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;

    if cur_token == expect {
        return Ok(());
    } else {
        return Err(ParserError::UnexpectedToken(cur_token));
    }
}


// primary_expression
// 	: IDENTIFIER
// 	| CONSTANT
// 	| STRING_LITERAL
// 	| '(' expression ')'
// 	;
fn parse_primary(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError>{
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::Identifier(_) | Token::IntegerLiteral(_) => {
            return Ok(Box::new(Node::Terminal(cur_token)));
        }

        Token::LParen => {
            let expr = parse_expr(lexer, 0)?;
            expect_token(lexer, Token::RParen)?; 
            return Ok(expr);
        }

        //TODO: String literal
        _ => return Err(ParserError::UnexpectedToken(cur_token)),
    }  
}

// postfix_expression
// 	: primary_expression
// 	| postfix_expression '[' expression ']'
// 	| postfix_expression '(' ')'
// 	| postfix_expression '(' argument_expression_list ')'
// 	| postfix_expression '.' IDENTIFIER
// 	| postfix_expression PTR_OP IDENTIFIER
// 	| postfix_expression INC_OP
// 	| postfix_expression DEC_OP
fn parse_postfix(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    
    // The common left-recursion issue can be circumvented by realizing that
    // a postfix expression must begin with a primary expression
    let mut primary : NodePtr = parse_primary(lexer)?;


    // ... followed by an arbitrary amount of postfix operators.
    loop {
        let cur_token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;

        match cur_token {
            Token::LBracket => { // Array Index
                lexer.next();
                let index = parse_expr(lexer, 0)?;
                expect_token(lexer, Token::RBracket)?;
                let new_primary = Node::ArrayIndex(ArrayIndexNode { lvalue : primary, index });
                primary = NodePtr::new(new_primary);
            }

            Token::Increment | Token::Decrement => {
                let operator = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;

                let new_primary = Node::Postfix(UnaryNode{ operator, operand : primary });
                primary = NodePtr::new(new_primary);
            }
            
            _ => return Ok(primary),
        }
    }

    // TODO: We need to loop postfix operators here.
}

// unary_expression
// 	: postfix_expression
// 	| INC_OP unary_expression
// 	| DEC_OP unary_expression
// 	| unary_operator cast_expression
// 	| SIZEOF unary_expression
// 	| SIZEOF '(' type_name ')'
fn parse_unary(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError>{
   
    match lexer.peek().ok_or(ParserError::EarlyLexerTermination)? {
        Token::IntegerLiteral(n) => {
            // The weird ordering here is because n is a reference to the peeked
            // value, thus .next cannot be called before.
            let node = Node::Terminal(Token::IntegerLiteral(*n));
            lexer.next();
            return Ok(Box::new(node));
        }

        Token::Increment | Token::Decrement | Token::Ampersand | Token::Asterisk | 
        Token::Plus | Token::Minus | Token::Negation => {
            //TODO: casting. Currently this calls a unary expression
            // but a unary expression does not allow for casting.
            let token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
            let operand = parse_unary(lexer)?;

            let node = Node::Prefix(
                UnaryNode { 
                    operator : token,
                    operand,
                });
            
            return Ok(Box::new(node));
        }

        Token::Sizeof => {
            // TODO: Evaluate sizeof as a constant immediately!
            lexer.next();
            let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
            if cur_token == Token::LParen {
                // Sizeof on type
                return Err(ParserError::NotImplemented);
                /*
                let type_token = parse_type(lexer)?;
                expect_token(lexer, Token::RParen);

                let prefix_node = Node::Prefix(
                        UnaryNode {
                            operator : Token::Sizeof,
                            operand : type_token
                        }
                    )

                return Ok( NodePtr::new( Node::Prefix(Node::Terminal(type_token))));
                */

            } else {
                // Sizeof also works on expressions! e.g. sizeof 123
                let operand = parse_unary(lexer)?;

                let node = Node::Prefix(
                    UnaryNode { 
                        operator : Token::Sizeof,
                        operand,
                    });
                
                return Ok(Box::new(node));
            }
        }

        _ => {
            return parse_postfix(lexer);
        }
    }
}

pub fn parse_expr(lexer : &mut Peekable<Lexer>, min_precedence : u8) -> Result<NodePtr, ParserError>{
    // Find next atom
    let mut expr : NodePtr = parse_unary(lexer)?;
    
    loop {
        let peek_token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;

        if let Ok(op_info) = operator_lookup(peek_token) {
            
            if op_info.precedence < min_precedence {
                return Ok(expr);
            }

            let op_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;

            let rhs : NodePtr;
            if op_info.left_associative {
                rhs = parse_expr(lexer, min_precedence + 1)?;
            } else { 
                rhs = parse_expr(lexer, min_precedence)?;
            }

            // By splitting the creation of the new expr node into two steps we avoid using Jones' trick.
            let new_expr = Node::Infix(BinaryNode{operator : op_token, left : expr, right : rhs});
            expr = NodePtr::new (new_expr);
        } else {
            // Peek token not recognised as operator - this is not an error,
            // as we may simply have run into the end of the expression.
            return Ok(expr);
        }
    }
}

//initializer
//	: assignment_expression
//	| '{' initializer_list '}'
//	| '{' initializer_list ',' '}'
//	;
fn parse_initializer(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    match lexer.peek().ok_or(ParserError::EarlyLexerTermination)? {
        Token::LCurlyBracket => {
            lexer.next();
            // TODO: Implement initializer_list

            return Err(ParserError::NotImplemented);
        }

        _ => return parse_expr(lexer, 0),
    }
}



/*
Seems like the only ways to tell apart a function_definition and a 
declaration is by 
1. Looking if it ends with a { or ; (this will always work)
or
2. Since a declaration can be an init_declarator it can contain an assignment
(e.g. a '='), which the declarator that the function_definition uses cannot.
However a declaration could also just be a declarator, in which case you cannot tell
the difference.

Strategy: Clearly, untill we reach the compound_statement a declaration
is a superset of a function_definition. As such, parse under the assumption
we have a declaration and convert it to a function definition if necessary.
*/


// external_declaration
// 	: function_definition
// 	| declaration
// 	;

fn parse_type_specifier(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let cur_token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::Float | Token::Int | Token::Double => {
            let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
            return Ok(NodePtr::new(Node::TypeSpecifier(cur_token)));
        }
        _ => {
            // Handle user defined types
            return Err(ParserError::NotImplemented);
        }
    }
}
// function_definition (without K&R syntax)
// 	| declaration_specifiers declarator compound_statement
// 	| declarator compound_statement (int is default return type)
// 	;

fn parse_declaration_specifier(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let cur_token = lexer.peek().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        // Token::Typedef | Token::Extern | Token::Static | Token::Auto | Token::Register
        Token::Const | Token::Volatile => { // type qualifer
            let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
            return Ok( NodePtr::new(Node::TypeQualifier(cur_token)));
        }

        _ => return parse_type_specifier(lexer),
    }
}



// declaration_specifiers
// 	: storage_class_specifier
// 	| storage_class_specifier declaration_specifiers
// 	| type_specifier
// 	| type_specifier declaration_specifiers
// 	| type_qualifier
// 	| type_qualifier declaration_specifiers
// 	;
// TODO: Storage Class specifiers ignored for now
fn parse_declaration_specifiers(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    // We require at least one declaration specifier
    let specifier = parse_declaration_specifier(lexer)?;
    
    let mut specifier_list = vec![specifier];
    loop {
        // Parse as many specifiers as possible
        if let Ok(specifier) = parse_declaration_specifier(lexer) {
            specifier_list.push(specifier);
        } else {
            return Ok(NodePtr::new(Node::DeclarationSpecifiers(specifier_list)))
        }
    }
}


// direct_declarator and declarator are tightly coupled as the name suggests
// The only reason the two are split into two is to ensure that the derived
// types [] (array of) and () (function) have a higher precedence than * (pointer to).
// As such, direct_declarator is only ever referenced externally by declarator and
// we can combine the parsing of the two into a single function for better 
// readability.

// direct_declarator
// 	: IDENTIFIER
// 	| '(' declarator ')'
// 	| direct_declarator '[' constant_expression ']'
// 	| direct_declarator '[' ']'
// 	| direct_declarator '(' parameter_type_list ')'
// 	| direct_declarator '(' identifier_list ')'
// 	| direct_declarator '(' ')'

// declarator
// 	: pointer direct_declarator
// 	| direct_declarator
fn parse_declarator(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    // TODO: implement pointer derived type
    
    //
    let base: NodePtr;
    let cur_token = lexer.next().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::Identifier(_) => {
            base = NodePtr::new(Node::Terminal(cur_token)); 
        }
        Token::LParen => {
            base = parse_declarator(lexer)?;
            expect_token(lexer, Token::RParen)?;
        }
        _ => return Err(ParserError::UnexpectedToken(cur_token)),
    }

    
    // Each "layer" of a declarator can only contain one derived type:
    // 1. [Base] is an array of ...
    // 2. [Base] is a function returning...
    // The former of two can be repeated more than once in a declarator layer,
    // the latter cannot.
    match lexer.peek().ok_or(ParserError::EarlyLexerTermination)? {
        Token::LParen => { // function declaration
            lexer.next();
            let postfix_derived_type = NodePtr::new(Node::Empty);

            // TODO: Implement parameter_type_list and identifier_list
            expect_token(lexer, Token::RParen).map_err(|_| ParserError::NotImplemented)?;
            
            let declarator_node = Node::FunctionDeclarator(DeclaratorNode{ base, postfix_derived_type});
            return Ok(NodePtr::new(declarator_node));
        }
        Token::LBracket => {
            // TODO: Implement array declaration.
            return Err(ParserError::NotImplemented);
        }
        _ => return Ok(base),
    }

}


//init_declarator
//	: declarator
//	| declarator '=' initializer
//	;
fn parse_init_declarator(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    // TODO: implement initializer
    let declarator = parse_declarator(lexer)?;
    match lexer.peek().ok_or(ParserError::EarlyLexerTermination)? {
        Token::Assign => {
            lexer.next();
            let initializer = parse_initializer(lexer)?;
            let init_declarator = Node::InitDeclarator(InitDeclaratorNode{ declarator, initializer});
            return Ok(NodePtr::new(init_declarator));
        }

        _ => {

            print_ast(declarator.clone(), "".to_string(), true);
            return Ok(declarator);
        }
    }
}

//init_declarator_list
//	: init_declarator
//	| init_declarator_list ',' init_declarator
//	;

fn parse_init_declarator_list(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    // TODO: Implement init_declarator_list
    return parse_init_declarator(lexer);
}


// declaration
// 	: declaration_specifiers ';'
// 	| declaration_specifiers init_declarator_list ';'
// 	;
// e.g. void; int a = 3;
pub fn parse_declaration(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let declaration_specifiers : NodePtr = parse_declaration_specifiers(lexer)?;
    let init_declarator_list : NodePtr;

    match lexer.peek().ok_or(ParserError::EarlyLexerTermination)? {
        Token::Semicolon => {
            lexer.next();
            init_declarator_list = NodePtr::new(Node::Empty);
        }
        _ => {
            init_declarator_list = parse_init_declarator_list(lexer)?;
        }
    }

    return Ok(NodePtr::new(Node::Declaration(DeclarationNode{declaration_specifiers, init_declarator_list} ) ));
}



