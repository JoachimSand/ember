use std::fmt::Formatter;
use std::{iter::Peekable, fmt, error::Error};
use crate::{lexer::*, b_red};
use crate::arena::*;
use crate::colours::*;
use crate::compile::CompilationError;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UnaryNode<'n> {
    operator : Token<'n>,
    operand : &'n Node<'n>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DerivedType<'n> {
    Pointer{ is_const : bool, is_volatile : bool },
    Array { size : Option<i64> },
    Function { parameter_list : &'n Node<'n> },
    FunctionParameterless,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Enumerator<'n> {
    name : &'n str,
    val : Option<i32>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Specifier<'n> {
    StorageClass(Token<'n>),
    TypeQualifier(Token<'n>),
    BasicType(Token<'n>),
    Struct{ is_union : bool, name: Option<&'n str>, declaration_list : Option<&'n [&'n StructDeclarationNode<'n>]>},
    Enum{ name: Option<&'n str>, enumerator_list : Option<&'n [&'n Enumerator<'n>]>}
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarationNode<'n> {
    pub specifier_qualifier_list : &'n SpecifierList<'n>,
    // we don't support bit fields: a struct declarator is the exact same as a normal declarator
    pub struct_declarator_list : &'n [&'n DeclaratorNode<'n>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct SpecifierList<'n> {
    pub specifiers : &'n [&'n Specifier<'n>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclaratorNode<'n> {
    pub name : &'n str,
    pub derived_types : &'n [DerivedType<'n>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct InitDeclaratorNode<'n> {
    pub declarator : &'n DeclaratorNode<'n>, 
    pub initializer : &'n Node<'n>
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IfNode<'n> {
    pub condition : &'n Node<'n>,
    pub statement : &'n Node<'n>
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Node<'n> {
    TranslationalUnit{external_declarations : &'n [&'n Node<'n>]},
    FunctionDefinition{ declaration_specifiers : &'n SpecifierList<'n>, declarator : &'n DeclaratorNode<'n>, compound_statement : &'n Node<'n>},
    
    CompoundStatement{ declaration_list : &'n Node<'n>, statement_list : &'n Node<'n>},
    DeclarationList(&'n [&'n Node<'n>]),
    StatementList(&'n [&'n Node<'n>]),

    Declaration{ declaration_specifiers : &'n SpecifierList<'n>, init_declarator_list : &'n Node<'n>},
    InitDeclarator(&'n InitDeclaratorNode<'n>),
    InitDeclaratorList(&'n [&'n InitDeclaratorNode<'n>]),
    InitializerList(&'n [&'n Node<'n>]),

    IfStatementList(&'n [&'n IfNode<'n>]),
    WhileStatement{ condition : &'n Node<'n>, statement : &'n Node<'n> },
    DoWhileStatement{ statement : &'n Node<'n>, condition : &'n Node<'n>},
    ForStatement{ init_statement : &'n Node<'n>, condition : &'n Node<'n>, iter_statement: &'n Node<'n>},

    FunctionCall{ function : &'n Node<'n>, arguments : &'n [&'n Node<'n>]},
    ArrayIndex{ lvalue : &'n Node<'n>, index : &'n Node<'n>},
    
    Infix{ operator : Token<'n>, left : &'n Node<'n>, right : &'n Node<'n> },
    Prefix(UnaryNode<'n>),
    Postfix(UnaryNode<'n>),

    Goto{ goto_id : &'n str},
    Return{ expression : &'n Node<'n>},
    Identifier{ name: &'n str },
    Literal(Token<'n>),
    Empty,
}



// wrapper functions for easy conversion to parser errors
fn next_token<'l, 't : 'l>(lexer : &'l mut Peekable<Lexer<'t>>) -> Result<Token<'t>, CompilationError<'t>> 
{
    lexer.next().ok_or(CompilationError::EarlyLexerTermination)
}

fn peek_token<'l, 't : 'l>(lexer : &'l mut Peekable<Lexer<'t>>) -> Result<&'l Token<'t>, CompilationError<'t>> 
{
    lexer.peek().ok_or(CompilationError::EarlyLexerTermination)
}

struct OperatorInfo {
    precedence : u8,
    left_associative : bool,
}
// Lookup table for binary operators
// https://en.cppreference.com/w/c/language/operator_precedence
fn operator_lookup<'t>(token : &'t Token) -> Result<OperatorInfo, CompilationError<'t>> {
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

        _ => return Result::Err(CompilationError::UnknownPrecedence(token.clone())),
    }
}

fn expect_token<'l, 't : 'l>(lexer : &'l mut Peekable<Lexer<'t>>, expect : Token) -> Result<Token<'t>, CompilationError<'t>> 
{
    let cur_token = next_token(lexer)?;

    if std::mem::discriminant(&cur_token) == std::mem::discriminant(&expect) {
        return Ok(cur_token);
    } else {
        return Err(CompilationError::UnexpectedToken(cur_token));
    }
}

// primary_expression
// 	: IDENTIFIER
// 	| CONSTANT
// 	| STRING_LITERAL
// 	| '(' expression ')'
// 	;
//
pub fn parse_primary<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> 
{
    let cur_token : Token = next_token(lexer)?;
    match cur_token {
        Token::Identifier(name) => {
            let node = arena.push(Node::Identifier{ name })?;
            return Ok(node);
        }

        Token::IntegerLiteral(_) | Token::FloatLiteral(_) | Token::DoubleLiteral(_) | Token::StringLiteral(_) => {
            let node = arena.push(Node::Literal(cur_token))?;
            return Ok(node);
        }

        Token::LParen => {
            let expr = parse_expr(lexer, arena, 0)?;
            expect_token(lexer, Token::RParen)?; 
            return Ok(expr);
        }

        _ => return Err(CompilationError::UnexpectedToken(cur_token)),
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
fn parse_postfix<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>>
{
    // The common left-recursion issue can be circumvented by realizing that
    // a postfix expression must begin with a primary expression
    let mut primary : &Node = parse_primary(lexer, arena)?;

    // ... followed by an arbitrary amount of postfix operators.
    loop {
        let cur_token = peek_token(lexer)?;

        match cur_token {
            
            Token::LBracket => { // Array Index
                next_token(lexer)?;
                let index = parse_expr(lexer, arena, 0)?;
                expect_token(lexer, Token::RBracket)?;
                let new_primary = Node::ArrayIndex{ lvalue : primary, index };
                primary = arena.push(new_primary)?;
            }

            Token::LParen => { // Function Call
                next_token(lexer)?;
                // TODO: Parse argument expressions
                
                let arguments : &[&Node];
                if let Token::RParen = peek_token(lexer)? {
                    next_token(lexer)?;
                    arguments = arena.push_slice_copy(&[])?;
                } else {
                    let mut argument_list = vec![parse_expr(lexer, arena, 0)?];
                    while let Token::Comma = peek_token(lexer)? {
                        next_token(lexer)?;
                        argument_list.push(parse_expr(lexer, arena, 0)?);
                    }
                    arguments = arena.push_slice_copy(&argument_list[..])?;
                    expect_token(lexer, Token::RParen)?;
                }
                
                let new_primary = Node::FunctionCall{ function : primary, arguments };
                primary = arena.push(new_primary)?;
            }

            Token::Increment | Token::Decrement => {
                let operator = next_token(lexer)?;

                let new_primary = Node::Postfix(UnaryNode{ operator, operand : primary });
                primary = arena.push(new_primary)?;
            }
            
            _ => return Ok(primary),
        }
    }
}

// unary_expression
// 	: postfix_expression
// 	| INC_OP unary_expression
// 	| DEC_OP unary_expression
// 	| unary_operator cast_expression
// 	| SIZEOF unary_expression
// 	| SIZEOF '(' type_name ')'

fn parse_unary<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>>
{
    let peek_token = peek_token(lexer)?;
    match peek_token {
        Token::Increment | Token::Decrement | Token::Ampersand | Token::Asterisk | 
        Token::Plus | Token::Minus | Token::Negation => {
            //TODO: casting. Currently this calls a unary expression
            // but a unary expression does not allow for casting.
            let token = next_token(lexer)?;
            let operand = parse_unary(lexer, arena)?;

            let node = Node::Prefix(
                UnaryNode { 
                    operator : token,
                    operand,
                });
            
            return arena.push(node);
        }

        Token::Sizeof => {
            // TODO: Evaluate sizeof as a constant immediately!
            next_token(lexer)?;
            let cur_token = next_token(lexer)?;
            if cur_token == Token::LParen {
                // Sizeof on type
                return Err(CompilationError::NotImplemented);
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
                let operand = parse_unary(lexer, arena)?;

                let node = Node::Prefix(
                    UnaryNode { 
                        operator : Token::Sizeof,
                        operand,
                    });
                
                return arena.push(node);
            }
        }

        _ => {
            return parse_postfix(lexer, arena);
        }
    }
}

fn parse_expr<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena, min_precedence : u8) -> Result<&'arena Node<'arena>, CompilationError<'arena>>
{
    // Find next atom
    let mut expr : &Node = parse_unary(lexer, arena)?;
    
    loop {
        let peek_token = peek_token(lexer)?;

        if let Ok(op_info) = operator_lookup(&peek_token) {
            
            if op_info.precedence < min_precedence {
                return Ok(expr);
            }

            let op_token = next_token(lexer)?;

            let rhs : &Node;
            if op_info.left_associative {
                rhs = parse_expr(lexer, arena, min_precedence + 1)?;
            } else { 
                rhs = parse_expr(lexer, arena, min_precedence)?;
            }

            // By splitting the creation of the new expr node into two steps we avoid using Jones' trick.
            let new_expr = Node::Infix{operator : op_token, left : expr, right : rhs};
            expr = arena.push(new_expr)?;
        } else {
            // Peek token not recognised as operator - this is not an error,
            // as we may simply have run into the end of the expression.
            return Ok(expr);
        }
    }
}


fn eval_const_expression<'e>(node : &Node) -> Result<i64, CompilationError<'e>> {
    match &*node {
        Node::Literal(token) => {
            if let Token::IntegerLiteral(num) = *token {
                return Ok(num);
            } else {
                return Err(CompilationError::InvalidConstExpression);
            }
        }

        Node::Infix{ operator, left, right } => {
            match operator {
                Token::Plus => return Ok( eval_const_expression(left)? + eval_const_expression(right)?),
                Token::Minus => return Ok( eval_const_expression(left)? - eval_const_expression(right)?),

                _ => return Err(CompilationError::InvalidConstExpression),
            }

        }

        _ => return Err(CompilationError::InvalidConstExpression), 
    }

}

//struct_declarator
//	: declarator
//	| ':' constant_expression BITFIELD 
//	| declarator ':' constant_expression BITFIELD
// Bitfields are not implemented - a struct declarator is just treated as a normal declarator

//struct_declarator_list
//	: struct_declarator
//	| struct_declarator_list ',' struct_declarator

//struct_declaration
//	: specifier_qualifier_list struct_declarator_list ';'
fn parse_struct_declaration<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena StructDeclarationNode<'arena>, CompilationError<'arena>> {
    let specifier_qualifier_list = parse_specifier_qualifier_list(lexer, arena)?;

    let declarator = parse_declarator(lexer, arena)?;
    let mut declarator_list = vec![declarator];

    while let Token::Comma = peek_token(lexer)? {
        next_token(lexer)?;
        declarator_list.push(parse_declarator(lexer, arena)?);
    }

    expect_token(lexer, Token::Semicolon)?;

    let struct_declarator_list = arena.push_slice_copy(&declarator_list[..])?;
    let node = StructDeclarationNode{ specifier_qualifier_list, struct_declarator_list};
    return Ok(arena.push(node)?);
}

//struct_or_union_specifier
//	: struct_or_union IDENTIFIER '{' struct_declaration_list '}'
//	| struct_or_union '{' struct_declaration_list '}'
//	| struct_or_union IDENTIFIER

//struct_or_union
//	: STRUCT
//	| UNION
fn parse_struct_or_union_specifier<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Specifier<'arena>, CompilationError<'arena>> {

    let is_union : bool;

    let mut cur_token = next_token(lexer)?;
    match cur_token {
        Token::Struct => is_union = false,
        Token::Union => is_union = true,
        _ => return Err(CompilationError::UnexpectedToken(cur_token)),
    }

    let mut name: Option<& str> = None;
    cur_token = next_token(lexer)?;
    if let Token::Identifier(id_name) = cur_token {
        name = Some(id_name);

        if let Token::LCurlyBracket = *peek_token(lexer)? { 
            cur_token = next_token(lexer)?;
        }
    }

    let mut declaration_list : Option<&[&StructDeclarationNode]> = None;
    //struct_declaration_list
    //;	: struct_declaration
    //;	| struct_declaration_list struct_declaration
    if let Token::LCurlyBracket = cur_token {
        let declaration = parse_struct_declaration(lexer, arena)?;
        let mut list = vec![declaration];
        while let Ok(declaration) = parse_struct_declaration(lexer, arena) {
            list.push(declaration);
        }

        declaration_list = Some(arena.push_slice_copy(&list[..])?);
        expect_token(lexer, Token::RCurlyBracket)?;
    } 

    let specifier = arena.push(Specifier::Struct { is_union, name, declaration_list })?;
    return Ok(specifier);
}

fn parse_enumerator<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Enumerator<'arena>, CompilationError<'arena>>{
    let mut enumerator : Enumerator = Enumerator { name: "", val: None };

    let cur_token = next_token(lexer)?;
    if let Token::Identifier(id_name) = cur_token {
        enumerator.name = id_name;
    } else {
        return Err(CompilationError::UnexpectedToken(cur_token));
    }

    if let Token::Assign = peek_token(lexer)? {
        next_token(lexer)?;
        let const_expr = parse_expr(lexer, arena, 0)?;
        enumerator.val = Some(eval_const_expression(const_expr)? as i32);
    } else {
        enumerator.val = None;
    }

    return Ok(arena.push(enumerator)?);
}

//enum_specifier
//	: ENUM '{' enumerator_list '}'
//	| ENUM IDENTIFIER '{' enumerator_list '}'
//	| ENUM IDENTIFIER
fn parse_enum_specifier<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Specifier<'arena>, CompilationError<'arena>>{
    expect_token(lexer, Token::Enum)?;

    let mut name: Option<& str> = None;
    
    let mut cur_token = next_token(lexer)?;
    if let Token::Identifier(id_name) = cur_token {
        name = Some(id_name);

        if let Token::LCurlyBracket = *peek_token(lexer)? { 
            cur_token = next_token(lexer)?;
        }
    }
 
    //enumerator_list
    //	: enumerator
    //	| enumerator_list ',' enumerator
    if let Token::LCurlyBracket = cur_token {
        let enumerator = parse_enumerator(lexer, arena)?;
        let mut enumerator_list = vec![enumerator];
        
        while let Token::Comma = peek_token(lexer)? {
            next_token(lexer)?;
            enumerator_list.push(parse_enumerator(lexer, arena)?);
        }
        expect_token(lexer, Token::RCurlyBracket)?;

        let enumerator_list = Some(arena.push_slice_copy(&enumerator_list[..])?);
        let specifier = Specifier::Enum { name, enumerator_list };
        return Ok(arena.push(specifier)?);
    } else {
        let specifier = Specifier::Enum { name, enumerator_list : None };
        return Ok(arena.push(specifier)?);
    }
}

//type_specifier
//	: VOID
//	| CHAR
//	| SHORT
//	| INT
//	| LONG
//	| FLOAT
//	| DOUBLE
//	| SIGNED
//	| UNSIGNED
//	| struct_or_union_specifier
//	| enum_specifier
//	| TYPE_NAME
//	;
fn parse_type_specifier<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Specifier<'arena>, CompilationError<'arena>> {
    let cur_token = peek_token(lexer)?;
    match cur_token {
        Token::Void | Token::Char | Token::Short | Token::Int | Token::Long | Token::Float |
        Token::Double | Token::Signed | Token::Unsigned => {
            let cur_token = next_token(lexer)?;
            let type_specifier = arena.push(Specifier::BasicType(cur_token))?;
            return Ok(type_specifier);
        }

        Token::Struct | Token::Union => {
            return parse_struct_or_union_specifier(lexer, arena);
        }

        Token::Enum => {
            return parse_enum_specifier(lexer, arena);
        }

        _ => {
            // Handle user defined types
            return Err(CompilationError::UnexpectedToken(*cur_token));
        }
    }
}

//specifier_qualifier_list
//	: type_specifier specifier_qualifier_list
//	| type_specifier
//	| type_qualifier specifier_qualifier_list
//	| type_qualifier
fn parse_specifier_qualifier_list<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena SpecifierList<'arena>, CompilationError<'arena>> {
    parse_declaration_specifiers(lexer, arena, false)
}
// declaration_specifiers
// 	: storage_class_specifier
// 	| storage_class_specifier declaration_specifiers
// 	| type_specifier
// 	| type_specifier declaration_specifiers
// 	| type_qualifier
// 	| type_qualifier declaration_specifiers
fn parse_declaration_specifier<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena, parse_storage_specifiers : bool) -> Result<&'arena Specifier<'arena>, CompilationError<'arena>> {
    let cur_token = peek_token(lexer)?;
    match cur_token {
        // Type qualifiers
        Token::Const | Token::Volatile => { // type qualifier
            let cur_token = next_token(lexer)?;
            let node = arena.push(Specifier::TypeQualifier(cur_token))?;
            return Ok(node);
        }
        
        // Storage class specifiers
        Token::Typedef | Token::Extern | Token::Static | Token::Auto | Token::Register => {
            if parse_storage_specifiers == false {
                return Err(CompilationError::UnexpectedToken(*cur_token));
            }

            let cur_token = next_token(lexer)?;
            let node = arena.push(Specifier::StorageClass(cur_token))?;
            return Ok(node);
        }

        _ => return parse_type_specifier(lexer, arena),
    }
}

fn parse_declaration_specifiers<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena, parse_storage_specifiers : bool) -> Result<&'arena SpecifierList<'arena>, CompilationError<'arena>> {
    // We require at least one declaration specifier
    let specifier = parse_declaration_specifier(lexer, arena, parse_storage_specifiers)?;
    
    let mut specifier_vec = vec![specifier];
    loop {
        // Parse as many specifiers as possible
        if let Ok(specifier) = parse_declaration_specifier(lexer, arena, parse_storage_specifiers) {
            specifier_vec.push(specifier);
        } else {
            let specifiers = arena.push_slice_copy(&specifier_vec[..])?;
            let node = arena.push(SpecifierList{specifiers})?;
            return Ok(node);
        }
    }
}


//pointer
//	: '*'
//	| '*' type_qualifier_list
//	| '*' pointer
//	| '*' type_qualifier_list pointer
// TODO: Add suport for const/volatile

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
fn parse_declarator_recursive<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena, derived_types : &mut Vec<DerivedType>, name : &mut&'arena str) -> Result<(), CompilationError<'arena>> {
    
    // parse pointer derived types but don't push them to list of derived types on the way down
    // the declarator tree
    let mut pointer_derived_types = Vec::<DerivedType>::new();
    loop {
        match peek_token(lexer)? {
            Token::Const => {
                let p = pointer_derived_types.last_mut().ok_or(CompilationError::UnexpectedToken(Token::Const))?;
                
                if let DerivedType::Pointer{ is_volatile, .. } = *p {
                    *p = DerivedType::Pointer{ is_const : true, is_volatile };
                }
            }

            Token::Volatile => {
                let p = pointer_derived_types.last_mut().ok_or(CompilationError::UnexpectedToken(Token::Const))?;
                
                if let DerivedType::Pointer{ is_const, .. } = *p {
                    *p = DerivedType::Pointer{ is_const, is_volatile : true };
                }
            }
            
            Token::Asterisk => {
                pointer_derived_types.push(DerivedType::Pointer{ is_const : false, is_volatile : false});
            }
            _ => break,
        }
        next_token(lexer)?;
    }


    let cur_token = next_token(lexer)?;
    match cur_token {
        Token::Identifier(id_name) => {
            *name = id_name;
        }
        Token::LParen => {
            parse_declarator_recursive(lexer, arena, derived_types, name)?;
            expect_token(lexer, Token::RParen)?;
        }
        _ => return Err(CompilationError::UnexpectedToken(cur_token)),
    }

    // Each "layer" of a declarator can only contain one derived type:
    // 1. [Base] is an array of ...
    // 2. [Base] is a function returning...
    // The former of the two can be repeated more than once in a declarator layer,
    // the latter cannot.

    loop {
        match peek_token(lexer)? {
            Token::LParen => { // function declaration
                next_token(lexer)?;

                // TODO: Implement parameter_type_list and identifier_list
                expect_token(lexer, Token::RParen).map_err(|_| CompilationError::NotImplemented)?;
                derived_types.push(DerivedType::FunctionParameterless);
                
            }
            Token::LBracket => {
                next_token(lexer)?;
                if let Token::RBracket = peek_token(lexer)? {
                    next_token(lexer)?;
                    derived_types.push(DerivedType::Array{ size : None});
                } else {
                    let constant_expression = parse_expr(lexer, arena, 0)?;
                    let size = eval_const_expression(constant_expression)?;
                    derived_types.push(DerivedType::Array{ size: Some(size)});
                    expect_token(lexer, Token::RBracket)?;
                }
            }
            _ => break,
        }
    }

    // ... finally push the prefix derived types which were parsed on the way down
    derived_types.append(&mut pointer_derived_types);

    return Ok(());
}

fn parse_declarator<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena DeclaratorNode<'arena>, CompilationError<'arena>> {
    let mut name = "";
    let mut derived_types_list : Vec<DerivedType> = Vec::new();

    parse_declarator_recursive(lexer, arena, &mut derived_types_list, &mut name)?;
    //println!("Declarator {:?}, {:?}", name, derived_types_list);

    let derived_types = arena.push_slice_copy(&derived_types_list[..])?;
    let declarator = arena.push(DeclaratorNode{name, derived_types})?;
    return Ok(declarator);
}

//initializer_list
//	: initializer
//	| initializer_list ',' initializer
//	;
fn parse_initializer_list<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let initializer = parse_initializer(lexer, arena)?;

    let mut list = vec![initializer];

    while let Token::Comma = peek_token(lexer)? {
        next_token(lexer)?;
        // An initializer_list can end with a trailing comma.
        if let Token::LCurlyBracket = peek_token(lexer)? {
            break;
        } else {
            list.push(parse_initializer(lexer, arena)?);
        }
    }

    let slice = arena.push_slice_copy(&list[..])?;
    let node = Node::InitializerList(slice);
    return Ok(arena.push(node)?);
}


//initializer
//	: assignment_expression
//	| '{' initializer_list '}'
//	| '{' initializer_list ',' '}'
//	;
fn parse_initializer<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    match peek_token(lexer)? {
        Token::LCurlyBracket => {
            next_token(lexer)?;
            let initializer_list = parse_initializer_list(lexer, arena)?;
            expect_token(lexer, Token::RCurlyBracket)?;
            return Ok(initializer_list);
        }

        _ => return parse_expr(lexer, arena, 0),
    }
}

//init_declarator
//	: declarator
//	| declarator '=' initializer
//	;
fn parse_init_declarator<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena InitDeclaratorNode<'arena>, CompilationError<'arena>> {
    let declarator = parse_declarator(lexer, arena)?;
    match peek_token(lexer)? {
        Token::Assign => {
            next_token(lexer);
            let initializer = parse_initializer(lexer, arena)?;
            let init_declarator = InitDeclaratorNode{ declarator, initializer};
            return Ok(arena.push(init_declarator)?);
        }

        _ => {

            let init_declarator = InitDeclaratorNode{ declarator, initializer : arena.push(Node::Empty)? };
            return Ok(arena.push(init_declarator)?);
        },
    }
}

//init_declarator_list
//	: init_declarator
//	| init_declarator_list ',' init_declarator
fn parse_init_declarator_list<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    // TODO: Implement init_declarator_list
    let init_declarator = parse_init_declarator(lexer, arena)?;
    let mut list = vec![init_declarator];
    while let Token::Comma = *peek_token(lexer)? {
        next_token(lexer);
        list.push(parse_init_declarator(lexer, arena)?);
    }

    if list.len() == 1 {
        // if we only encountered one init declarator, just return the single init declarator on it's own
        return Ok(arena.push(Node::InitDeclarator(init_declarator))?);
    } else {
        let list = arena.push_slice_copy(&list[..])?;
        let node = arena.push(Node::InitDeclaratorList(list))?;
        return Ok(node);
    }
}

// declaration
// 	: declaration_specifiers ';'
// 	| declaration_specifiers init_declarator_list ';'
// 	;
// e.g. void; int a = 3;
pub fn parse_declaration<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena, consume_semicolon : bool) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let declaration_specifiers = parse_declaration_specifiers(lexer, arena, true)?;
    let init_declarator_list : &Node;

    if Token::Semicolon == *peek_token(lexer)? { 
        init_declarator_list = arena.push(Node::Empty)?;
    } else {
        init_declarator_list = parse_init_declarator_list(lexer, arena)?;
    }

    if consume_semicolon {
        expect_token(lexer, Token::Semicolon)?;
    }

    let declaration = arena.push(Node::Declaration{declaration_specifiers, init_declarator_list})?;
    return Ok(declaration);
}



//declaration_list
//	: declaration
//	| declaration_list declaration
//	;
fn parse_declaration_list<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let declaration = parse_declaration(lexer, arena, true)?;
    let mut list = vec![declaration]; 

    // Keep pushing declarations untill we encounter an error 
    loop {
        match parse_declaration(lexer, arena, true) {
            Ok(node) => {
                list.push(node);
            }
            Err(_) => {
                let declarations = arena.push_slice_copy(&list[..])?;
                let node = Node::DeclarationList(declarations);
                return Ok(arena.push(node)?);
            }
        }
    }
}


pub fn parse_expr_statement<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    match peek_token(lexer)? {
        Token::Semicolon => {
            next_token(lexer)?;
            return Ok(arena.push(Node::Empty)?);
        }
        _ => {
            let expr = parse_expr(lexer, arena, 0)?;
            expect_token(lexer, Token::Semicolon)?;
            return Ok(expr);
        }
    }
}
//statement
//	: labeled_statement
//	| compound_statement
//	| expression_statement
//	| selection_statement
//	| iteration_statement
//	| jump_statement
//	;
pub fn parse_statement<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let peek = peek_token(lexer)?;

    match peek {
        //selection_statement
        //	: IF '(' expression ')' statement
        //	| IF '(' expression ')' statement ELSE statement
        //	| SWITCH '(' expression ')' statement
        Token::If => {
            let mut if_vec = Vec::<&IfNode>::new();

            // Rather than parsing if statements recursively,
            // we attempt to parse all if statements on the same "level"
            // into a single list which can be iterated over. This should
            // make SSA/codegen easier.
            loop {
                next_token(lexer)?; // consume if
                expect_token(lexer, Token::LParen)?;
                let condition = parse_expr(lexer, arena, 0)?;
                expect_token(lexer, Token::RParen)?;
                let statement = parse_statement(lexer, arena)?;
                let if_node = arena.push(IfNode{ condition, statement })?;
                if_vec.push(if_node);

                if let Token::Else = peek_token(lexer)? {
                    next_token(lexer)?;

                    if let Token::If = peek_token(lexer)? {
                        // 
                        continue;
                    } else {
                        // Just an else with no condition: this ends the series of
                        // if-else statements
                        let else_statement = parse_statement(lexer, arena)?;
                        let else_condition = arena.push(Node::Empty)?;
                        let else_node = arena.push(IfNode{condition : else_condition, statement: else_statement})?;
                        if_vec.push(else_node);
                        break;
                    }
                } else {
                    break;
                } 
            }

            let if_slice = arena.push_slice_copy(&if_vec[..])?;
            let node = Node::IfStatementList(if_slice);
            return Ok(arena.push(node)?);


                
        }
        //jump_statement
        //    : GOTO IDENTIFIER ';'
        //    | CONTINUE ';'
        //    | BREAK ';'
        //    | RETURN ';'
        //    | RETURN expression ';'

        Token::Continue | Token::Break => {
            return Err(CompilationError::NotImplemented);
            //let node = Node::Terminal(*peek_token);
            //expect_token(lexer, Token::Semicolon);
            //return Ok(NodePtr::new(node));
        }

        Token::Goto => {
            next_token(lexer)?;
            // TODO: A lot of these .ok_or() could be a lot more succint with a function
            let possible_id_token = next_token(lexer)?;
            match possible_id_token {
                Token::Identifier(name) => {
                    let node = Node::Goto{ goto_id : name };
                    expect_token(lexer, Token::Semicolon)?;
                    return Ok(arena.push(node)?);
                }
                _ => return Err(CompilationError::UnexpectedToken(possible_id_token)),
            }
        }

        Token::Return => {
            next_token(lexer)?;
            match peek_token(lexer)? {
                Token::Semicolon => {
                    next_token(lexer)?;
                    let node = Node::Return{expression : arena.push(Node::Empty)?};
                    return Ok(arena.push(node)?);
                }

                _ => {
                    let expression = parse_expr(lexer, arena, 0)?;
                    expect_token(lexer, Token::Semicolon)?;
                    let node = Node::Return { expression };
                    return Ok(arena.push(node)?);
                }
            }
        }

        //labeled_statement
        //	: IDENTIFIER ':' statement
        //	| CASE constant_expression ':' statement
        //	| DEFAULT ':' statement
        // TODO: Support the topmost production rule.
        // This is difficult since both expression and labeled_statement can
        // start with an identifier.
        /*Token::Case => {

        }*/

        //iteration_statement
        //	: WHILE '(' expression ')' statement
        //	| DO statement WHILE '(' expression ')' ';'
        //	| FOR '(' expression_statement expression_statement ')' statement
        //	| FOR '(' expression_statement expression_statement expression ')' statement
        //	;

        Token::While => {
            next_token(lexer)?;
            expect_token(lexer, Token::LParen)?;
            let condition = parse_expr(lexer, arena, 0)?;

            expect_token(lexer, Token::RParen)?;
            let statement = parse_statement(lexer, arena)?;
            let node = Node::WhileStatement { condition, statement };
            return Ok(arena.push(node)?);
        }

        Token::Do => {
            next_token(lexer)?;
            let statement = parse_statement(lexer, arena)?;

            expect_token(lexer, Token::While)?;
            expect_token(lexer, Token::LParen)?;
            let condition = parse_expr(lexer, arena, 0)?;
            expect_token(lexer, Token::RParen)?;
            expect_token(lexer, Token::Semicolon)?;
            
            let node = Node::DoWhileStatement { condition, statement };
            return Ok(arena.push(node)?);
        }

        Token::For => {
            next_token(lexer)?;
            expect_token(lexer, Token::LParen)?;
            let init_statement = parse_expr_statement(lexer, arena)?;
            let condition = parse_expr_statement(lexer, arena)?;
            let iter_statement = if let Ok(expr) = parse_expr(lexer, arena, 0) {
                expr
            } else {
                arena.push(Node::Empty)?
            };
            expect_token(lexer, Token::RParen)?;

            let node = Node::ForStatement { init_statement, condition, iter_statement };
            return Ok(arena.push(node)?);
        }

        Token::LCurlyBracket => return parse_compound_statement(lexer, arena),

        _ => {
            // No applicable keywords encountered, try parsing expression
            return parse_expr_statement(lexer, arena);
        }
    }
}


fn parse_statement_list<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let statement = parse_statement(lexer, arena)?;
    let mut list = vec![statement]; 

    loop { 
        match parse_statement(lexer, arena) {
            Ok(node) => {
                list.push(node);
            }
            Err(_) => {
                let statements = arena.push_slice_copy(&list[..])?;
                let node = arena.push(Node::StatementList(statements))?;
                return Ok(node);
            }
        }
    }
}

//compound_statement
//	: '{' '}'
//	| '{' statement_list '}'
//	| '{' declaration_list '}'
//	| '{' declaration_list statement_list '}'
//	;
fn parse_compound_statement<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    expect_token(lexer, Token::LCurlyBracket)?;
   
    let mut declarations = Vec::<&Node>::new(); 
    loop {
        match parse_declaration(lexer, arena, true) {
            Ok(node) => {
                declarations.push(node);
            }
            Err(_) => break,
        }
    }

    //println!("Parsed declaration_list");

    let mut statements = Vec::<&Node>::new(); 
    loop {
        match parse_statement(lexer, arena) {
            Ok(node) => {
                statements.push(node);
            }
            Err(_) => break,
        }
    }

    let statements = arena.push_slice_copy(&statements[..])?;
    let statement_list = arena.push(Node::StatementList(statements))?;

    let declarations = arena.push_slice_copy(&declarations[..])?;
    let declaration_list = arena.push(Node::DeclarationList(declarations))?;
    let compound_statement = Node::CompoundStatement{ declaration_list, statement_list};
    
    return Ok(arena.push(compound_statement)?);
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

Strategy: Clearly, until we reach the compound_statement a declaration
is a superset of a function_definition. As such, parse under the assumption
we have a declaration and convert it to a function definition if necessary.
*/

// function_definition (without K&R syntax)
// 	| declaration_specifiers declarator compound_statement
// 	| declarator compound_statement (int is default return type) TODO: Support this syntax
// 	;

// external_declaration
// 	: function_definition
// 	| declaration
// 	;
fn parse_external_declaration<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let declaration = parse_declaration(lexer, arena, false)?;    
   
    let peek_token = peek_token(lexer)?; 
    match peek_token {
        Token::Semicolon => {
            // Semicolon encountered - this external declaration must be a declaration.
            next_token(lexer);
            return Ok(declaration);
        }

        Token::LCurlyBracket => {
            // Left curly bracket - this external declaration must be a function_definition
            // we must now "extract" the declaration_specifiers and declarator from
            // the previously parsed declaration
            let func_declaration_specifiers : &SpecifierList;
            let func_declarator : &DeclaratorNode;

            if let Node::Declaration{ declaration_specifiers, init_declarator_list} = *declaration {
                func_declaration_specifiers = declaration_specifiers;

                if let Node::InitDeclarator(init_node) = *init_declarator_list {
                    let first_type = init_node.declarator.derived_types.last().ok_or(CompilationError::UnableToDecomposeDeclaration)?;
                    match first_type {
                        DerivedType::Function{ .. } | DerivedType::FunctionParameterless => func_declarator = init_node.declarator,
                        _ => return Err(CompilationError::UnableToDecomposeDeclaration),
                    }
                } else {

                    return Err(CompilationError::UnableToDecomposeDeclaration);
                }
                /*
                if let Node::Declarator(DeclaratorNode{ref derived_types, ..}) = *init_declarator_list {
                    let first_type = derived_types.last().ok_or(CompilationError::UnableToDecomposeDeclaration)?;
                    match first_type {
                        DerivedType::Function{ .. } | DerivedType::FunctionParameterless => func_declarator = init_declarator_list,
                        _ => return Err(CompilationError::UnableToDecomposeDeclaration),
                    }

                } else {
                    return Err(CompilationError::UnableToDecomposeDeclaration);
                }*/
            } else {
                return Err(CompilationError::UnableToDecomposeDeclaration);
            }

            let compound_statement = parse_compound_statement(lexer, arena)?;
            let func_definition = Node::FunctionDefinition{ declaration_specifiers : func_declaration_specifiers, declarator : func_declarator, compound_statement};
            return Ok(arena.push(func_definition)?);
        }

        _ => {
            return Err(CompilationError::UnexpectedToken(peek_token.clone()));
        }
    }
}

//translation_unit
//	: external_declaration
//	| translation_unit external_declaration
//	;
pub fn parse_translational_unit<'arena>(lexer : &mut Peekable<Lexer<'arena>>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {

    // a translation_unit must have at least one external_declaration according to the grammar
    let first_declaration = parse_external_declaration(lexer, arena)?;
    let mut external_declarations = vec![first_declaration];

    while lexer.peek().is_some() {
        let declaration = parse_external_declaration(lexer, arena)?;
        external_declarations.push(declaration);
    }

    let external_declarations = arena.push_slice_copy(&external_declarations[..])?;
    let translation_unit = Node::TranslationalUnit { external_declarations };
    return Ok(arena.push(translation_unit)?);

}
