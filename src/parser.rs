use std::{iter::Peekable, fmt, error::Error};
use crate::lexer::*;
use crate::arena::*;

#[derive(Debug)]
pub enum ParserError {
    NotImplemented,
    UnexpectedToken(Token),
    EarlyLexerTermination,
    UnknownPrecedence(Token),
    UnableToDecomposeDeclaration,
    InvalidConstExpression,
    AllocationError,
}

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Parser Error: {}", self.to_string())
    }
}

// ? operator will automatically call this for conversion.
impl From<ArenaError> for ParserError {
    fn from(e: ArenaError) -> Self {
        return ParserError::AllocationError;
    }
}


// wrapper functions for easy conversion to parser errors
fn next_token(lexer : &mut Peekable<Lexer>) -> Result<Token, ParserError> {
    lexer.next().ok_or(ParserError::EarlyLexerTermination)
}

fn peek_token<'l, 't>(lexer : &'l mut Peekable<Lexer>) -> Result<&'t Token, ParserError> 
where 'l : 't
{
    lexer.peek().ok_or(ParserError::EarlyLexerTermination)
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryNode<'u> {
    operator : Token,
    operand : &'u Node<'u>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DerivedType<'n> {
    Pointer,
    Array { size : Option<i64> },
    Function { parameter_list : &'n Node<'n> },
    FunctionParameterless,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationSpecifiersNode<'n> {
    pub specifiers : &'n [Token],
}

impl<'n> ArenaSupported for DeclarationSpecifiersNode<'n> {}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclaratorNode<'n> {
    pub name : String,
    pub derived_types : Vec<DerivedType<'n>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'n> {
    //TranslationalUnit{external_declarations : Vec<NodePtr>},
    //FunctionDefinition{ declaration_specifiers : Box<DeclarationSpecifiersNode>, declarator : Box<DeclaratorNode>, compound_statement : NodePtr},
    //CompoundStatement{ declaration_list : NodePtr, statement_list : NodePtr},

    //DeclarationList(Vec<NodePtr>),
    //Declaration{ declaration_specifiers : Box<DeclarationSpecifiersNode>, init_declarator_list : NodePtr},
    //DeclarationSpecifiers(DeclarationSpecifiersNode),

    //StatementList(Vec<NodePtr>),
    Goto{ goto_id : String},
    Return{ expression : &'n Node<'n>},

    InitDeclarator{ declarator : &'n DeclaratorNode<'n>, initializer : &'n Node<'n>},

    TypeQualifier(Token),
    TypeSpecifier(Token),
    _StorageSpecifier(Token),
    
    Infix{ operator : Token, left : &'n Node<'n>, right : &'n Node<'n> },
    Prefix(UnaryNode<'n>),
    Postfix(UnaryNode<'n>),

    FunctionCall{ function_name : &'n Node<'n>, arguments : &'n Node<'n>},
    ArrayIndex{ lvalue : &'n Node<'n>, index : &'n Node<'n>},
    
    Identifier{ name: String },
    Literal(Token),
    Empty,
}

impl ArenaSupported for &[Token] {}
impl<'n> ArenaSupported for Node<'n> {}

impl<'n> fmt::Display for Node<'n> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            /*
            Node::TranslationalUnit{..} => write!(f, "Translational Unit"),
            Node::FunctionDefinition{..} => write!(f, "Function Definition"),
            Node::CompoundStatement{..} => write!(f, "Compound Statement"),
            Node::DeclarationList(_) => write!(f, "Declaration List"),
            Node::StatementList(_) => write!(f, "Statement List"),

            Node::InitDeclarator{..} => write!(f, "Init Declarator"),
            /*
            Node::Declarator(declarator) => {
                write!(f, "Declarator {}, ", declarator.name)?;

                let mut iter = declarator.derived_types.iter().peekable();

                while let Some(node) = iter.next() {
                    match node {
                        DerivedType::Pointer => { write!(f, "* ")?; }
                        DerivedType::FunctionParameterless => { write!(f, "() ")?; }
                        DerivedType::Array { size }=> { 
                            if let Some(size) = size {
                                write!(f, "[{}] ", size)?;
                            } else {
                                write!(f, "[] ")?;
                            }
                        }
                        _ => { write!(f, "Unimplemented")?; }
                    }
                    ()
                }
                write!(f, "")
            } */

            Node::Declaration{..} => write!(f, "Declaration"),
            Node::DeclarationSpecifiers(_) => write!(f, "Declaration Specifiers"),
            Node::TypeSpecifier(t) => write!(f, "{:?}", t),
            Node::TypeQualifier(t) => write!(f, "{:?}", t),
            Node::_StorageSpecifier(t) => write!(f, "{:?}", t),
            
            Node::Infix(n) => write!(f, "I{:?}", n.operator),
            Node::Prefix(n) => write!(f, "Prefix {:?}", n.operator),
            Node::Postfix(n) => write!(f, "Postfix {:?}", n.operator),
            Node::ArrayIndex(_) => write!(f, "Array Index"),
            Node::FunctionCall(_) => write!(f, "Function Call"),
            Node::Literal(t) => write!(f, "{:?}", t),
            Node::Identifier{name} => write!(f, "{}", name),

            Node::Empty => write!(f, "Empty"),
            */
            _ => write!(f, "Unknown Node"),

        }
    }
}

struct OperatorInfo {
    precedence : u8,
    left_associative : bool,
}

pub fn print_ast(start : &'static Node<'static>, prefix : String, is_last : bool) {
    print!("{}", prefix);
    let mut new_prefix : String = prefix;
    if is_last {
        print!("└──");
        new_prefix.push_str("    ");
    } else {
        print!("├──");
        new_prefix.push_str("│  ");
    }

    println!("{}", start);
    
    match *start {
        /*
        Node::FunctionDefinition{ declaration_specifiers, declarator, compound_statement} => {
            //print_ast(declaration_specifiers, new_prefix.clone(), false);
            //print_ast(declarator, new_prefix.clone(), false);
            print_ast(compound_statement, new_prefix, true);
        }

        Node::CompoundStatement{declaration_list, statement_list} => {
            print_ast(declaration_list, new_prefix.clone(), false);
            print_ast(statement_list, new_prefix, true);
        }

        // quite possibly the coolest Rust syntax I have encountered so far
        Node::DeclarationList(list) | Node::StatementList(list) | Node::TranslationalUnit { external_declarations: list @ _ }=> {
            let mut iter = list.iter().peekable();

            while let Some(node) = iter.next() {
                if iter.peek().is_some() {
                    print_ast(node.clone(), new_prefix.clone(), false);
                } else {
                    print_ast(node.clone(), new_prefix.clone(), true);
                }
            }
        }

        Node::Empty | Node::Identifier{..} | Node::Literal(_) | Node::TypeSpecifier(_) | Node::TypeQualifier(_) => {
            return;
        }
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

        Node::Declaration{ declaration_specifiers, init_declarator_list} => {
            //print_ast(declaration_specifiers, new_prefix.clone(), false);
            print_ast(init_declarator_list, new_prefix, true);
        }

        Node::InitDeclarator{ declarator, initializer} => {
            //print_declarator(declarator, new_prefix.clone(), false);
            print_ast(initializer, new_prefix, true);
        }
        */
        _ => {
            print!("Print AST node not implemented for {:?}", *start);
            return;
        }
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
fn expect_token(lexer : &mut Peekable<Lexer>, expect : Token) -> Result<Token, ParserError> {
    let cur_token = next_token(lexer)?;

    if std::mem::discriminant(&cur_token) == std::mem::discriminant(&expect) {
        return Ok(cur_token);
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
//

// Read life times as: Given a reference valid for lifetime 'n to a parser
// return a reference which is valid for lifetime 'n to a node. Both the lexer.and node may
// only generate references to themselves which last for lifetime 'n.
pub fn parse_primary<'arena>(lexer : &mut Peekable<Lexer>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, ParserError> 
{
    let cur_token : Token = next_token(lexer)?;
    match cur_token {
        Token::Identifier(name) => {
            let node = arena.push(Node::Identifier{ name })?;
            return Ok(node);
        }

        Token::IntegerLiteral(_) | Token::FloatLiteral(_) | Token::DoubleLiteral(_) => {
            let node = arena.push(Node::Literal(cur_token))?;
            return Ok(node);
        }

        Token::LParen => {
            let expr = parse_expr(lexer, arena, 0)?;
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
fn parse_postfix<'arena>(lexer : &mut Peekable<Lexer>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, ParserError> 
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

                expect_token(lexer, Token::RParen)?;
                let new_primary = Node::FunctionCall{ function_name : primary, arguments : arena.push(Node::Empty)? };
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

    // TODO: We need to loop postfix operators here.
}

// unary_expression
// 	: postfix_expression
// 	| INC_OP unary_expression
// 	| DEC_OP unary_expression
// 	| unary_operator cast_expression
// 	| SIZEOF unary_expression
// 	| SIZEOF '(' type_name ')'

fn parse_unary<'arena>(lexer : &mut Peekable<Lexer>, arena : &'arena Arena) -> Result<&'arena Node<'arena>, ParserError> 
{
    let peek_token = peek_token(lexer)?;
    match peek_token {
        /*
        Token::IntegerLiteral(_) | Token::FloatLiteral(_) | Token::DoubleLiteral(_) => {
            let token = lexer.next_token().ok_or(ParserError::EarlyLexerTermination)?;
            let node = Node::Literal(token);
            return Ok(Box::new(node));
        }
        */

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
            
            return Ok(arena.push(node)?);
        }

        Token::Sizeof => {
            // TODO: Evaluate sizeof as a constant immediately!
            next_token(lexer)?;
            let cur_token = next_token(lexer)?;
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
                let operand = parse_unary(lexer, arena)?;

                let node = Node::Prefix(
                    UnaryNode { 
                        operator : Token::Sizeof,
                        operand,
                    });
                
                return Ok(arena.push(node)?);
            }
        }

        _ => {
            return parse_postfix(lexer, arena);
        }
    }
}

fn parse_expr<'arena>(lexer : &mut Peekable<Lexer>, arena : &'arena Arena, min_precedence : u8) -> Result<&'arena Node<'arena>, ParserError>
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


fn eval_const_expression(node : &Node) -> Result<i64, ParserError> {
    match &*node {
        Node::Literal(token) => {
            if let Token::IntegerLiteral(num) = *token {
                return Ok(num);
            } else {
                return Err(ParserError::InvalidConstExpression);
            }
        }

        Node::Infix{ operator, left, right } => {
            match operator {
                Token::Plus => return Ok( eval_const_expression(left)? + eval_const_expression(right)?),
                Token::Minus => return Ok( eval_const_expression(left)? - eval_const_expression(right)?),

                _ => return Err(ParserError::InvalidConstExpression),
            }

        }

        _ => return Err(ParserError::InvalidConstExpression), 
    }

}

/*
//initializer
//	: assignment_expression
//	| '{' initializer_list '}'
//	| '{' initializer_list ',' '}'
//	;
fn parse_initializer(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    match lexer.peek_token() {
        Token::LCurlyBracket => {
            lexer.next_token();
            // TODO: Implement initializer_list

            return Err(ParserError::NotImplemented);
        }

        _ => return parse_expr(lexer, 0),
    }
}*/

fn parse_type_specifier(lexer : &mut Peekable<Lexer>) -> Result<Token, ParserError> {
    let cur_token = peek_token(lexer)?;
    match cur_token {
        Token::Float | Token::Int | Token::Double => {
            let cur_token = next_token(lexer);
            return Ok(cur_token?);
        }
        _ => {
            // Handle user defined types
            return Err(ParserError::NotImplemented);
        }
    }
}

fn parse_declaration_specifier(lexer : &mut Peekable<Lexer>) -> Result<Token, ParserError> {
    let cur_token = peek_token(lexer)?;
    match cur_token {
        // Token::Typedef | Token::Extern | Token::Static | Token::Auto | Token::Register
        Token::Const | Token::Volatile => { // type qualifer
            let cur_token = next_token(lexer)?;
            return Ok(cur_token);
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
// TODO: Storage Class specifiers ignored for no


fn parse_declaration_specifiers<'arena>(lexer : &mut Peekable<Lexer>, arena : &'arena Arena) -> Result<&'arena DeclarationSpecifiersNode<'arena>, ParserError> {
    // We require at least one declaration specifier
    let specifier = parse_declaration_specifier(lexer)?;
    
    let mut specifier_vec = vec![specifier];
    loop {
        // Parse as many specifiers as possible
        if let Ok(specifier) = parse_declaration_specifier(lexer) {
            specifier_vec.push(specifier);
        } else {
            let specifiers = arena.push_slice_copy(&specifier_vec[..])?;
            let node = arena.push(DeclarationSpecifiersNode{specifiers})?;
            return Ok(node);
        }
    }
}

/*
//pointer
//	: '*'
//	| '*' type_qualifier_list
//	| '*' pointer
//	| '*' type_qualifier_list pointer
// TODO: Add suport for const/volatile
/*
fn parse_pointer(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let list = vec![expect_token(lexer, Token::Asterisk)?];

    while let Token::Asterisk = lexer.peek_token() {
        list.push(Token::Asterisk)
    }

    return Ok(NodePtr::new(Node::Pointer(list)));
}*/

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
fn parse_declarator_recursive(lexer : &mut Peekable<Lexer>, declarator : &mut DeclaratorNode) -> Result<(), ParserError> {
    
    // parse pointer derived types but don't push them to list of derived types on the way down
    // the declarator tree
    let mut pointer_count = 0;
    while let Token::Asterisk = lexer.peek_token() {
        lexer.next_token();
        pointer_count += 1;
    }

    let cur_token = lexer.next_token().ok_or(ParserError::EarlyLexerTermination)?;
    match cur_token {
        Token::Identifier(name) => {
            declarator.name = name;
        }
        Token::LParen => {
            parse_declarator_recursive(lexer, declarator)?;
            expect_token(lexer, Token::RParen)?;
        }
        _ => return Err(ParserError::UnexpectedToken(cur_token)),
    }


    
    // Each "layer" of a declarator can only contain one derived type:
    // 1. [Base] is an array of ...
    // 2. [Base] is a function returning...
    // The former of the two can be repeated more than once in a declarator layer,
    // the latter cannot.

    loop {
        match lexer.peek_token() {
            Token::LParen => { // function declaration
                lexer.next_token();

                // TODO: Implement parameter_type_list and identifier_list
                expect_token(lexer, Token::RParen).map_err(|_| ParserError::NotImplemented)?;
                declarator.derived_types.push(DerivedType::FunctionParameterless);
                
            }
            Token::LBracket => {
                lexer.next_token();
                if let Token::RBracket = lexer.peek_token() {
                    lexer.next_token();
                    declarator.derived_types.push(DerivedType::Array{ size : None});
                } else {
                    let constant_expression = parse_expr(lexer, 0)?;
                    let size = eval_const_expression(constant_expression)?;
                    declarator.derived_types.push(DerivedType::Array{ size: Some(size)});
                    expect_token(lexer, Token::RBracket)?;
                }
            }
            _ => break,
        }
    }

    // ... finally push the pointer derived types which were parsed on the way down
    for _ in 0..pointer_count {
        declarator.derived_types.push(DerivedType::Pointer);
    }

    return Ok(());
}

fn parse_declarator(lexer : &mut Peekable<Lexer>) -> Result<Box<DeclaratorNode>, ParserError> {
    let mut declarator = DeclaratorNode{ name : String::new(), derived_types : Vec::<DerivedType>::new()};
    parse_declarator_recursive(lexer, &mut declarator)?;

    return Ok(Box::new(declarator));
}


//init_declarator
//	: declarator
//	| declarator '=' initializer
//	;
fn parse_init_declarator(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let declarator = parse_declarator(lexer)?;
    match lexer.peek_token() {
        Token::Assign => {
            lexer.next_token();
            let initializer = parse_initializer(lexer)?;
            let init_declarator = Node::InitDeclarator{ declarator, initializer};
            return Ok(NodePtr::new(init_declarator));
        }

        _ => {

            let init_declarator = Node::InitDeclarator{ declarator, initializer : NodePtr::new(Node::Empty)};
            return Ok(NodePtr::new(init_declarator));
        },
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
fn parse_declaration(lexer : &mut Peekable<Lexer>, consume_semicolon : bool) -> Result<NodePtr, ParserError> {
    let declaration_specifiers = parse_declaration_specifiers(lexer)?;
    let init_declarator_list : NodePtr;

    if Token::Semicolon == *lexer.peek_token() { 
        init_declarator_list = NodePtr::new(Node::Empty);
    } else {
        init_declarator_list = parse_init_declarator_list(lexer)?;
    }

    if consume_semicolon {
        expect_token(lexer, Token::Semicolon)?;
    }

    return Ok(NodePtr::new(Node::Declaration {declaration_specifiers, init_declarator_list} ));
}

//declaration_list
//	: declaration
//	| declaration_list declaration
//	;
fn parse_declaration_list(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let declaration = parse_declaration(lexer, true)?;
    let mut list = vec![declaration]; 

    loop {
        match parse_declaration(lexer, true) {
            Ok(node) => {
                list.push(node);
            }
            Err(_) => return Ok(NodePtr::new(Node::DeclarationList(list)))
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
pub fn parse_statement(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let peek_token = lexer.peek_token();

    match peek_token {

        //jump_statement
        //    : GOTO IDENTIFIER ';'
        //    | CONTINUE ';'
        //    | BREAK ';'
        //    | RETURN ';'
        //    | RETURN expression ';'

        Token::Continue | Token::Break => {
            return Err(ParserError::NotImplemented);
            //let node = Node::Terminal(*peek_token);
            //expect_token(lexer, Token::Semicolon);
            //return Ok(NodePtr::new(node));
        }

        Token::Goto => {
            lexer.next_token();
            // TODO: A lot of these .ok_or() could be a lot more succint with a function
            let possible_id_token = lexer.next_token().ok_or(ParserError::EarlyLexerTermination)?;
            match possible_id_token {
                Token::Identifier(name) => {
                    let node = Node::Goto{ goto_id : name };
                    expect_token(lexer, Token::Semicolon)?;
                    return Ok(NodePtr::new(node));
                }
                _ => return Err(ParserError::UnexpectedToken(possible_id_token)),
            }
        }

        Token::Return => {
            lexer.next_token();
            let cur_token = lexer.next_token().ok_or(ParserError::EarlyLexerTermination)?;

            match cur_token {
                Token::Semicolon => {
                    let node = Node::Return{expression : NodePtr::new(Node::Empty)};
                    return Ok(NodePtr::new(node));
                }

                _ => {
                    let err_token = cur_token.clone();
                    let expression = parse_expr(lexer, 0).map_err(|_| ParserError::UnexpectedToken(err_token))?;
                    let node = Node::Return { expression };

                    return Ok(NodePtr::new(node));
                }
            }
        }

        Token::LCurlyBracket => return parse_compound_statement(lexer),
        

        //expression_statement
        //    : ';'
        //    | expression ';'
        //    ;
        Token::Semicolon => {
            lexer.next_token();
            return Ok(NodePtr::new(Node::Empty));
        }

        _ => {
            // No applicable keywords encountered, try 
            let expression = parse_expr(lexer, 0)?;
            expect_token(lexer, Token::Semicolon)?;
            return Ok(expression);
        }
    }
}


fn parse_statement_list(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let statement = parse_statement(lexer)?;
    let mut list = vec![statement]; 

    loop { 
        match parse_statement(lexer) {
            Ok(node) => {
                list.push(node);
            }
            Err(_) => return Ok(NodePtr::new(Node::StatementList(list)))
        }
    }
}


//compound_statement
//	: '{' '}'
//	| '{' statement_list '}'
//	| '{' declaration_list '}'
//	| '{' declaration_list statement_list '}'
//	;
fn parse_compound_statement(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    expect_token(lexer, Token::LCurlyBracket)?;
   
    let mut declaration_list = Vec::<NodePtr>::new(); 
    loop {
        match parse_declaration(lexer, true) {
            Ok(node) => {
                declaration_list.push(node);
            }
            Err(_) => break,
        }
    }

    println!("Parsed declaration_list");

    let mut statement_list = Vec::<NodePtr>::new(); 
    loop {
        match parse_statement(lexer) {
            Ok(node) => {
                statement_list.push(node);
            }
            Err(_) => break,
        }
    }

    let statement_list_node = NodePtr::new(Node::StatementList(statement_list));
    let declaration_list_node = NodePtr::new(Node::DeclarationList(declaration_list));
    let compound_statement = Node::CompoundStatement { declaration_list : declaration_list_node, statement_list : statement_list_node};
    
    return Ok(NodePtr::new(compound_statement));
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
fn parse_external_declaration(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {
    let declaration = parse_declaration(lexer, false)?;    
   
    let peek_token = lexer.peek_token(); 
    match peek_token {
        Token::Semicolon => {
            // Semicolon encountered - this external declaration must be a declaration.
            lexer.next_token();
            return Ok(declaration);
        }

        Token::LCurlyBracket => {
            // Left curly bracket - this external declaration must a function_definition
            // we must now "extract" the declaration_specifiers and declarator from
            // the previously parsed declaration
            let func_declaration_specifiers : Box<DeclarationSpecifiersNode>;
            let func_declarator : Box<DeclaratorNode>;

            if let Node::Declaration{ declaration_specifiers, init_declarator_list} = *declaration {
                func_declaration_specifiers = declaration_specifiers;

                if let Node::InitDeclarator{ declarator, initializer } = *init_declarator_list {
                    let first_type = declarator.derived_types.last().ok_or(ParserError::UnableToDecomposeDeclaration)?;
                    match first_type {
                        DerivedType::Function{ .. } | DerivedType::FunctionParameterless => func_declarator = declarator,
                        _ => return Err(ParserError::UnableToDecomposeDeclaration),
                    }
                } else {

                    return Err(ParserError::UnableToDecomposeDeclaration);
                }
                /*
                if let Node::Declarator(DeclaratorNode{ref derived_types, ..}) = *init_declarator_list {
                    let first_type = derived_types.last().ok_or(ParserError::UnableToDecomposeDeclaration)?;
                    match first_type {
                        DerivedType::Function{ .. } | DerivedType::FunctionParameterless => func_declarator = init_declarator_list,
                        _ => return Err(ParserError::UnableToDecomposeDeclaration),
                    }

                } else {
                    return Err(ParserError::UnableToDecomposeDeclaration);
                }*/
            } else {
                return Err(ParserError::UnableToDecomposeDeclaration);
            }

            let compound_statement = parse_compound_statement(lexer)?;
            let func_definition = Node::FunctionDefinition{ declaration_specifiers : func_declaration_specifiers, declarator : func_declarator, compound_statement};
            return Ok(NodePtr::new(func_definition));
        }

        _ => {
            return Err(ParserError::UnexpectedToken(peek_token.clone()));
        }
    }
}

//translation_unit
//	: external_declaration
//	| translation_unit external_declaration
//	;
pub fn parse_translational_unit(lexer : &mut Peekable<Lexer>) -> Result<NodePtr, ParserError> {

    // a translation_unit must have at least one external_declaration according to the grammar
    let first_declaration = parse_external_declaration(lexer)?;
    let mut external_declarations = vec![first_declaration];

    while lexer.peek().is_some() {
        let declaration = parse_external_declaration(lexer)?;
        external_declarations.push(declaration);
    }

    let translation_unit = Node::TranslationalUnit { external_declarations };
    return Ok(NodePtr::new(translation_unit));

}
*/
