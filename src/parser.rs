use crate::arena::*;
use crate::compile::CompilationError;
use crate::lexer::*;
use crate::typechecker::{Type, TypeAlias};

// TODO: Consider if all these tokens should contain Token or TokenType.
// Do we need debugging information in the built/fully parsed AST?

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UnaryNode<'n> {
    pub operator: Token<'n>,
    pub operand: &'n Node<'n>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DerivedType<'n> {
    Pointer {
        is_const: bool,
        is_volatile: bool,
    },
    Array {
        size: Option<i64>,
    },
    Function {
        parameter_type_list: &'n ParameterTypeList<'n>,
    },
    FunctionParameterless,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Enumerator<'n> {
    pub name: &'n str,
    pub val: Option<i32>,
}

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub enum TypeSpecifier<'n> {
    // Primitives
    // We follow LP64. That is, long and pointers are 64 bit while
    // integers are 32 bit.
    UnsignedChar,     // u8.  Must be able to hold numbers between 0-255.
    SignedChar,       // i8.  Must be able to hold nubmers between -127-128
    Short,            // i16. May be referred to as short, short int, signed short int, signed short
    UnsignedShort,    // u16. May be referred to as unsigned short, unsigned short int
    Int,              // i32. May be referred to as int, signed int or signed.
    UnsignedInt,      // u32. May be referred to as unsigned int, unsigned.
    Long,             // i64. May be referred to as long int, long, signed long int, signed long.
    UnsignedLong,     // u64. May be referred to as unsigned long, unsigned long int.
    LongLong, // i128. May be reffered to as long long, long long int, signed long long int, signed long long
    UnsignedLongLong, // u128. May be reffered to as unsigned long long, unsigned long long int.
    Float,    // f32
    Double,   // f64
    LongDouble, // f64
    Void,
    TypeAlias(&'n TypeAlias<'n>),

    Enum {
        name: Option<&'n str>,
        enumerator_list: Option<&'n [&'n Enumerator<'n>]>,
    },
    Struct {
        is_union: bool,
        name: Option<&'n str>,
        declaration_list: Option<&'n [&'n StructDeclarationNode<'n>]>,
    },
    #[default]
    None,
}

#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Specifiers<'n> {
    // Type qualifiers
    pub is_const: bool,
    pub is_volatile: bool,
    pub is_typedef: bool,

    // Storage class specifiers
    pub is_extern: bool,
    pub is_static: bool,

    pub type_specifier: TypeSpecifier<'n>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarationNode<'n> {
    pub specifiers: &'n Specifiers<'n>,
    // we don't support bit fields: a struct declarator is the exact same as a normal declarator
    pub struct_declarator_list: &'n [&'n DeclaratorNode<'n>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParamDeclaration<'n> {
    pub declaration_specifiers: &'n Specifiers<'n>,
    pub declarator: Option<&'n DeclaratorNode<'n>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParameterTypeList<'n> {
    pub param_declarations: &'n [&'n ParamDeclaration<'n>],
    pub has_ellipsis: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclaratorNode<'n> {
    pub name: Option<&'n str>,
    pub derived_types: &'n [DerivedType<'n>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct InitDeclaratorNode<'n> {
    pub declarator: &'n DeclaratorNode<'n>,
    pub initializer: &'n Node<'n>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IfNode<'n> {
    pub condition: &'n Node<'n>,
    pub statement: &'n Node<'n>,
}

// TODO: Why is PartialEq required?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Node<'n> {
    TranslationalUnit {
        external_declarations: &'n [&'n Node<'n>],
    },
    FunctionDefinition {
        declaration_specifiers: &'n Specifiers<'n>,
        declarator: &'n DeclaratorNode<'n>,
        compound_statement: &'n Node<'n>,
    },

    CompoundStatement {
        declaration_list: &'n Node<'n>,
        statement_list: &'n Node<'n>,
    },
    // TODO: These two do not need to be Node variants.
    DeclarationList(&'n [&'n Node<'n>]),
    StatementList(&'n [&'n Node<'n>]),

    Declaration {
        declaration_specifiers: &'n Specifiers<'n>,
        init_declarator_list: &'n Node<'n>,
    },
    InitDeclaratorList(&'n [&'n InitDeclaratorNode<'n>]),
    InitializerList(&'n [&'n Node<'n>]),

    IfStatementList(&'n [&'n IfNode<'n>]),
    WhileStatement {
        condition: &'n Node<'n>,
        statement: &'n Node<'n>,
    },
    DoWhileStatement {
        statement: &'n Node<'n>,
        condition: &'n Node<'n>,
    },
    ForStatement {
        init_statement: &'n Node<'n>,
        condition: &'n Node<'n>,
        iter_statement: &'n Node<'n>,
        statement: &'n Node<'n>,
    },

    FunctionCall {
        function: &'n Node<'n>,
        arguments: &'n [&'n Node<'n>],
    },
    ArrayIndex {
        lvalue: &'n Node<'n>,
        index: &'n Node<'n>,
    },

    Infix {
        operator: Token<'n>,
        left: &'n Node<'n>,
        right: &'n Node<'n>,
    },
    Prefix(UnaryNode<'n>),
    Postfix(UnaryNode<'n>),

    Goto {
        goto_id: &'n str,
    },
    Return {
        expression: &'n Node<'n>,
    },
    Identifier {
        name: &'n str,
    },
    Literal(Token<'n>),
    Empty,
}

// TODO: Convert into From trait
// wrapper functions for easy conversion to parser errors
fn next_token<'l, 't: 'l>(
    lexer: &'l mut Lexer<'t>, /*, arena : &'a Arena, expecting : &'a str*/
) -> Result<Token<'t>, CompilationError<'t>> {
    let t = lexer.next_token().ok_or(CompilationError::ExpectedInput);
    // println!("consumed {t:?}");
    t
}

fn peek_token<'l, 't: 'l>(
    lexer: &'l mut Lexer<'t>, /*, arena : &'a Arena, expecting : &'a str*/
) -> Result<Token<'t>, CompilationError<'t>> {
    let t = lexer.peek_token().ok_or(CompilationError::ExpectedInput);
    // println!("peekd {t:?}");
    t
}

struct OperatorInfo {
    precedence: u8,
    left_associative: bool,
}
// Lookup table for binary operators
// https://en.cppreference.com/w/c/language/operator_precedence
fn operator_lookup<'t, 'e>(token: &'t Token<'e>) -> Result<OperatorInfo, CompilationError<'e>> {
    match token.token_type {
        TokenType::Asterisk | TokenType::Div | TokenType::Mod => {
            return Result::Ok(OperatorInfo {
                precedence: 200,
                left_associative: true,
            });
        }

        TokenType::Plus | TokenType::Minus => {
            return Result::Ok(OperatorInfo {
                precedence: 190,
                left_associative: true,
            });
        }

        TokenType::LeftShift | TokenType::RightShift => {
            return Result::Ok(OperatorInfo {
                precedence: 180,
                left_associative: true,
            });
        }

        TokenType::LessThan
        | TokenType::LessThanOrEq
        | TokenType::GreaterThan
        | TokenType::GreaterThanOrEq => {
            return Result::Ok(OperatorInfo {
                precedence: 170,
                left_associative: true,
            });
        }

        TokenType::Equals | TokenType::NotEquals => {
            return Result::Ok(OperatorInfo {
                precedence: 160,
                left_associative: true,
            });
        }

        TokenType::Ampersand => {
            // bitwise AND
            return Result::Ok(OperatorInfo {
                precedence: 150,
                left_associative: true,
            });
        }

        TokenType::XOR => {
            // bitwise XOR
            return Result::Ok(OperatorInfo {
                precedence: 140,
                left_associative: true,
            });
        }

        TokenType::OR => {
            // bitwise OR
            return Result::Ok(OperatorInfo {
                precedence: 130,
                left_associative: true,
            });
        }

        TokenType::ANDLogical => {
            return Result::Ok(OperatorInfo {
                precedence: 120,
                left_associative: true,
            });
        }

        TokenType::ORLogical => {
            return Result::Ok(OperatorInfo {
                precedence: 110,
                left_associative: true,
            });
        }

        // TODO: Ternary
        TokenType::Assign
        | TokenType::PlusAssign
        | TokenType::MinusAssign
        | TokenType::MultAssign
        | TokenType::DivAssign
        | TokenType::ModAssign
        | TokenType::LeftShiftAssign
        | TokenType::RightShiftAssign
        | TokenType::ANDAssign
        | TokenType::XORAssign
        | TokenType::ORAssign => {
            return Result::Ok(OperatorInfo {
                precedence: 90,
                left_associative: false,
            });
        }

        _ => return Result::Err(CompilationError::UnknownOperator(token.clone())),
    }
}

fn expect_token<'l, 't: 'l>(
    lexer: &'l mut Lexer<'t>,
    expect: TokenType<'t>,
) -> Result<Token<'t>, CompilationError<'t>> {
    let cur_token = lexer
        .next_token()
        .ok_or(CompilationError::ExpectedToken(expect))?;

    if cur_token.token_type == expect {
        return Ok(cur_token);
    } else {
        return Err(CompilationError::UnexpectedToken(cur_token));
    }
}

fn expect_closing_delimiter<'l, 't: 'l>(
    lexer: &'l mut Lexer<'t>,
    opening_delimeter: Token<'t>,
    expected_delimeter: TokenType<'t>,
) -> Result<Token<'t>, CompilationError<'t>> {
    let mut cur_token = lexer
        .next_token()
        .ok_or(CompilationError::UnclosedDelimeter(opening_delimeter))?;

    // The current token is not the closing delimiter. However, we may simple have stopped parsing early and the delimeter may simply be further down.
    while cur_token.token_type != expected_delimeter {
        cur_token = lexer
            .next_token()
            .ok_or(CompilationError::UnclosedDelimeter(opening_delimeter))?;
    }

    Ok(cur_token)
}

type ParseResult<'arena> = Result<&'arena Node<'arena>, CompilationError<'arena>>;

// primary_expression
// 	: IDENTIFIER
// 	| CONSTANT
// 	| STRING_LITERAL
// 	| '(' expression ')'
// 	;
//
pub fn parse_primary<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> ParseResult<'arena> {
    let cur_token = next_token(lexer)?;
    match cur_token.token_type {
        TokenType::Identifier(name) => {
            let node = arena.push(Node::Identifier { name })?;
            return Ok(node);
        }

        TokenType::IntLiteral(_)
        | TokenType::LongLiteral(_)
        | TokenType::UIntLiteral(_)
        | TokenType::ULongLiteral(_)
        | TokenType::FloatLiteral(_)
        | TokenType::DoubleLiteral(_)
        | TokenType::CharLiteral(_)
        | TokenType::StringLiteral(_) => {
            let node = arena.push(Node::Literal(cur_token))?;
            return Ok(node);
        }

        TokenType::LParen => {
            let expr_res = parse_expr(lexer, arena, 0, &[TokenType::RParen]);
            expect_closing_delimiter(lexer, cur_token, TokenType::RParen)?;
            return Ok(expr_res?);
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
fn parse_postfix<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    // The common left-recursion issue can be circumvented by realizing that
    // a postfix expression must begin with a primary expression
    let mut primary: &Node = parse_primary(lexer, arena)?;

    // ... followed by an arbitrary amount of postfix operators.
    loop {
        let cur_token = peek_token(lexer)?;

        match cur_token.token_type {
            TokenType::LBracket => {
                // Array Index
                next_token(lexer)?;
                let index = parse_expr(lexer, arena, 0, &[TokenType::RBracket])?;
                expect_token(lexer, TokenType::RBracket)?;
                let new_primary = Node::ArrayIndex {
                    lvalue: primary,
                    index,
                };
                primary = arena.push(new_primary)?;
            }

            TokenType::LParen => {
                // Function Call
                let opening_paren = next_token(lexer)?;

                let mut parse_function_call =
                    || -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
                        let arguments: &[&Node];
                        if let TokenType::RParen = peek_token(lexer)?.token_type {
                            next_token(lexer)?;
                            arguments = arena.push_slice_copy(&[])?;
                        } else {
                            let accepted_terminators = [TokenType::Comma, TokenType::RParen];

                            let mut argument_list =
                                vec![parse_expr(lexer, arena, 0, &accepted_terminators)?];
                            while let TokenType::Comma = peek_token(lexer)?.token_type {
                                next_token(lexer)?;
                                argument_list.push(parse_expr(
                                    lexer,
                                    arena,
                                    0,
                                    &accepted_terminators,
                                )?);
                            }
                            arguments = arena.push_slice_copy(&argument_list[..])?;
                        }
                        let function_call = Node::FunctionCall {
                            function: primary,
                            arguments,
                        };
                        let function_node = arena.push(function_call)?;

                        Ok(function_node)
                    };

                // Ensure that delimiter errors "bubble up" first.
                let parse_func_call_res = parse_function_call();
                expect_closing_delimiter(lexer, opening_paren, TokenType::RParen)?;
                primary = parse_func_call_res?;
            }

            TokenType::Increment | TokenType::Decrement => {
                let operator = next_token(lexer)?;

                let new_primary = Node::Postfix(UnaryNode {
                    operator,
                    operand: primary,
                });
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

fn parse_unary<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let peek_token = peek_token(lexer)?;
    match peek_token.token_type {
        TokenType::Increment
        | TokenType::Decrement
        | TokenType::Ampersand
        | TokenType::Asterisk
        | TokenType::Plus
        | TokenType::Minus
        | TokenType::Negation => {
            //TODO: casting. Currently this calls a unary expression
            // but a unary expression does not allow for casting.
            let token = next_token(lexer)?;
            let operand = parse_unary(lexer, arena)?;

            let node = Node::Prefix(UnaryNode {
                operator: token,
                operand,
            });

            return arena.push(node);
        }

        TokenType::Sizeof => {
            // TODO: Evaluate sizeof as a constant immediately!
            let sizeof_tok = next_token(lexer)?;
            let cur_token = next_token(lexer)?;
            if cur_token.token_type == TokenType::LParen {
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

                let node = Node::Prefix(UnaryNode {
                    operator: sizeof_tok,
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

fn parse_expr<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    min_precedence: u8,
    accepted_terminators: &[TokenType],
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    // Find next atom
    let mut expr: &Node = parse_unary(lexer, arena)?;

    loop {
        let peek_token = peek_token(lexer)?;
        /*
        Removed usage of the "accepted" terminators, as the context which called parse_expr should handle
        the termination case.

        if accepted_terminators.contains(&peek_token.token_type) {
            // when we start parsing an expression, we should specify which token may succesfully "terminate an expression".
            // e.g. ] when parsing an array index expression.
            return Ok(expr);
        } else {*/

        if let Ok(op_info) = operator_lookup(&peek_token) {
            if op_info.precedence < min_precedence {
                return Ok(expr);
            }

            let op_token = next_token(lexer)?;

            let rhs: &Node;
            if op_info.left_associative {
                rhs = parse_expr(lexer, arena, min_precedence + 1, accepted_terminators)?;
            } else {
                rhs = parse_expr(lexer, arena, min_precedence, accepted_terminators)?;
            }

            // By splitting the creation of the new expr node into two steps we avoid using Jones' trick.
            let new_expr = Node::Infix {
                operator: op_token,
                left: expr,
                right: rhs,
            };
            expr = arena.push(new_expr)?;
        } else {
            return Ok(expr);
        }
    }
}

fn eval_const_expression<'e>(node: &Node) -> Result<i64, CompilationError<'e>> {
    match node {
        Node::Literal(token) => {
            if let TokenType::IntLiteral(num) = token.token_type {
                return Ok(num as i64);
            } else {
                return Err(CompilationError::InvalidConstExpression);
            }
        }

        Node::Infix {
            operator,
            left,
            right,
        } => match operator.token_type {
            TokenType::Plus => {
                return Ok(eval_const_expression(left)? + eval_const_expression(right)?)
            }
            TokenType::Minus => {
                return Ok(eval_const_expression(left)? - eval_const_expression(right)?)
            }

            _ => return Err(CompilationError::InvalidConstExpression),
        },

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
fn parse_struct_declaration<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena StructDeclarationNode<'arena>, CompilationError<'arena>> {
    let specifiers = parse_specifier_qualifier_list(lexer, arena)?;

    let declarator = parse_declarator(lexer, arena, specifiers, false)?;
    let mut declarator_list = vec![declarator];

    while let TokenType::Comma = peek_token(lexer)?.token_type {
        next_token(lexer)?;
        declarator_list.push(parse_declarator(lexer, arena, specifiers, false)?);
    }

    expect_token(lexer, TokenType::Semicolon)?;

    let struct_declarator_list = arena.push_slice_copy(&declarator_list[..])?;
    let node = StructDeclarationNode {
        specifiers,
        struct_declarator_list,
    };
    return Ok(arena.push(node)?);
}

//struct_or_union_specifier
//	: struct_or_union IDENTIFIER '{' struct_declaration_list '}'
//	| struct_or_union '{' struct_declaration_list '}'
//	| struct_or_union IDENTIFIER

//struct_or_union
//	: STRUCT
//	| UNION
macro_rules! STRUCT_OR_UNION_STARTER {
    () => {
        TokenType::Struct | TokenType::Union
    };
}

fn parse_struct_or_union_specifier<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<TypeSpecifier<'arena>, CompilationError<'arena>> {
    let is_union: bool;

    let mut cur_token = next_token(lexer)?;
    match cur_token.token_type {
        TokenType::Struct => is_union = false,
        TokenType::Union => is_union = true,
        _ => return Err(CompilationError::UnexpectedToken(cur_token)),
    }

    let mut name: Option<&str> = None;
    cur_token = next_token(lexer)?;
    if let TokenType::Identifier(id_name) = cur_token.token_type {
        name = Some(id_name);

        if let TokenType::LCurlyBracket = peek_token(lexer)?.token_type {
            cur_token = next_token(lexer)?;
        }
    }

    let mut declaration_list: Option<&[&StructDeclarationNode]> = None;
    //struct_declaration_list
    //;	: struct_declaration
    //;	| struct_declaration_list struct_declaration
    if let TokenType::LCurlyBracket = cur_token.token_type {
        let mut parse_decl_list = || -> Result<(), CompilationError<'arena>> {
            let declaration = parse_struct_declaration(lexer, arena)?;
            let mut list = vec![declaration];
            while let Ok(declaration) = parse_struct_declaration(lexer, arena) {
                list.push(declaration);
            }

            declaration_list = Some(arena.push_slice_copy(&list[..])?);
            Ok(())
        };
        let parse_decl_list_res = parse_decl_list();
        expect_closing_delimiter(lexer, cur_token, TokenType::RCurlyBracket)?;
        parse_decl_list_res?;
    }

    return Ok(TypeSpecifier::Struct {
        is_union,
        name,
        declaration_list,
    });
}

// enumerator
// 	: IDENTIFIER
// 	| IDENTIFIER '=' constant_expression
fn parse_enumerator<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Enumerator<'arena>, CompilationError<'arena>> {
    let mut enumerator: Enumerator = Enumerator {
        name: "",
        val: None,
    };

    let cur_token = next_token(lexer)?;
    if let TokenType::Identifier(id_name) = cur_token.token_type {
        enumerator.name = id_name;
    } else {
        return Err(CompilationError::UnexpectedToken(cur_token));
    }

    if let TokenType::Assign = peek_token(lexer)?.token_type {
        next_token(lexer)?;
        let const_expr = parse_expr(lexer, arena, 0, &[TokenType::Comma])?;
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
fn parse_enum_specifier<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<TypeSpecifier<'arena>, CompilationError<'arena>> {
    expect_token(lexer, TokenType::Enum)?;

    let mut name: Option<&str> = None;

    let mut cur_token = next_token(lexer)?;
    if let TokenType::Identifier(id_name) = cur_token.token_type {
        name = Some(id_name);

        if let TokenType::LCurlyBracket = peek_token(lexer)?.token_type {
            cur_token = next_token(lexer)?;
        }
    }

    //enumerator_list
    //	: enumerator
    //	| enumerator_list ',' enumerator
    if let TokenType::LCurlyBracket = cur_token.token_type {
        let mut parse_enumerator_list =
            || -> Result<TypeSpecifier<'arena>, CompilationError<'arena>> {
                let enumerator = parse_enumerator(lexer, arena)?;
                let mut enumerator_list = vec![enumerator];

                while let TokenType::Comma = peek_token(lexer)?.token_type {
                    next_token(lexer)?;
                    enumerator_list.push(parse_enumerator(lexer, arena)?);
                }
                let enumerator_list = Some(arena.push_slice_copy(&enumerator_list[..])?);
                let specifier = TypeSpecifier::Enum {
                    name,
                    enumerator_list,
                };
                return Ok(specifier);
            };

        let enumerator_list_res = parse_enumerator_list();
        expect_closing_delimiter(lexer, cur_token, TokenType::RCurlyBracket)?;
        Ok(enumerator_list_res?)
    } else {
        let specifier = TypeSpecifier::Enum {
            name,
            enumerator_list: None,
        };
        return Ok(specifier);
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
//

//specifier_qualifier_list
//	: type_specifier specifier_qualifier_list
//	| type_specifier
//	| type_qualifier specifier_qualifier_list
//	| type_qualifier
fn parse_specifier_qualifier_list<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Specifiers<'arena>, CompilationError<'arena>> {
    parse_declaration_specifiers(lexer, arena, false)
}
// declaration_specifiers
// 	: storage_class_specifier
// 	| storage_class_specifier declaration_specifiers
// 	| type_specifier
// 	| type_specifier declaration_specifiers
// 	| type_qualifier
// 	| type_qualifier declaration_specifiers

macro_rules! TYPE_STARTER {
    () => {
        TokenType::Void
            | TokenType::Char
            | TokenType::Short
            | TokenType::Int
            | TokenType::Long
            | TokenType::Float
            | TokenType::Double
            | TokenType::Signed
            | TokenType::Unsigned
            | TokenType::TypeName(_)
    };
}

macro_rules! TYPE_SPECIFIER_STARTER {
    () => {
        TYPE_STARTER!() | STRUCT_OR_UNION_STARTER!() | TokenType::Enum
    };
}

macro_rules! DECLARATION_SPECIFIER_STARTER {
    () => {
        TYPE_SPECIFIER_STARTER!()
            | TokenType::Const
            | TokenType::Volatile
            | TokenType::Typedef
            | TokenType::Extern
            | TokenType::Static
            | TokenType::Auto
            | TokenType::Register
    };
}

// TODO: While the error messages showing which specifiers cannot be combined is quite nice,
// it does complicate this code somewhat. Using bools instead of tokens would slightly simplify things.
fn parse_declaration_specifiers<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    parse_storage_specifiers: bool,
) -> Result<&'arena Specifiers<'arena>, CompilationError<'arena>> {
    let mut specifiers = Specifiers {
        is_const: false,
        is_volatile: false,
        is_typedef: false,
        is_extern: false,
        is_static: false,
        type_specifier: TypeSpecifier::None,
    };

    // State used to parse primitives
    let mut unsigned_t: Option<Token> = None;
    let mut signed_t: Option<Token> = None;
    let mut char_t: Option<Token> = None;
    let mut short_t: Option<Token> = None;
    let mut int_t: Option<Token> = None;
    let mut long_t: Option<Token> = None;
    let mut long_token_count = 0;
    let mut float_t: Option<Token> = None;
    let mut double_t: Option<Token> = None;
    let mut void_t: Option<Token> = None;
    let mut enum_t: Option<Token> = None;
    let mut struct_or_union_t: Option<Token> = None;
    let mut type_name_t: Option<Token> = None;

    let mut typedef_token: Option<Token> = None;
    let mut extern_token: Option<Token> = None;
    let mut static_token: Option<Token> = None;

    let mut parse_declaration_specifier = |lexer: &mut Lexer<'arena>,
                                           arena: &'arena Arena|
     -> Result<(), CompilationError> {
        let set_specifier_token = |lexer: &mut Lexer<'arena>,
                                   check_tok: &mut Option<Token<'arena>>,
                                   consume_token: bool|
         -> Result<(), CompilationError<'arena>> {
            let specifier = if consume_token {
                next_token(lexer)?
            } else {
                peek_token(lexer)?
            };
            if check_tok.is_some() {
                Err(CompilationError::CannotCombineDeclarationSpecifiers {
                    prev_specifier: check_tok.unwrap(),
                    specifier,
                })
            } else {
                *check_tok = Some(specifier);
                Ok(())
            }
        };

        let cur_token = peek_token(lexer)?;

        match cur_token.token_type {
            // Type qualifiers
            TokenType::Const => {
                next_token(lexer)?;
                specifiers.is_const = true;
                Ok(())
            }

            TokenType::Volatile => {
                next_token(lexer)?;
                specifiers.is_volatile = true;
                Ok(())
            }

            // Storage class specifiers
            TokenType::Typedef
            | TokenType::Extern
            | TokenType::Static
            | TokenType::Auto
            | TokenType::Register => {
                if parse_storage_specifiers == false {
                    return Err(CompilationError::UnexpectedToken(cur_token));
                }
                let cur_token = next_token(lexer)?;

                match cur_token.token_type {
                    TokenType::Typedef => typedef_token = Some(cur_token),
                    TokenType::Extern => extern_token = Some(cur_token),
                    TokenType::Static => static_token = Some(cur_token),
                    TokenType::Auto | TokenType::Register => return Err(CompilationError::NotImplemented),
                    _ => panic!("This can not happen. Missed storage class specifier in match-statement while parsing."),
                }
                Ok(())
            }

            // Type specifiers
            TokenType::Signed => set_specifier_token(lexer, &mut signed_t, true),

            TokenType::Unsigned => set_specifier_token(lexer, &mut unsigned_t, true),

            TokenType::Char => set_specifier_token(lexer, &mut char_t, true),

            TokenType::Short => set_specifier_token(lexer, &mut short_t, true),

            TokenType::Int => set_specifier_token(lexer, &mut int_t, true),

            TokenType::Long => {
                let specifier = next_token(lexer)?;
                if long_token_count > 1 {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: long_t.unwrap(),
                        specifier,
                    });
                } else {
                    long_token_count += 1;
                    long_t = Some(specifier);
                    Ok(())
                }
            }

            TokenType::Float => set_specifier_token(lexer, &mut float_t, true),

            TokenType::Double => set_specifier_token(lexer, &mut double_t, true),

            TokenType::Void => set_specifier_token(lexer, &mut void_t, true),

            TokenType::TypeName(_) => set_specifier_token(lexer, &mut type_name_t, true),

            TokenType::Struct | TokenType::Union => {
                set_specifier_token(lexer, &mut struct_or_union_t, false)?;
                specifiers.type_specifier = parse_struct_or_union_specifier(lexer, arena)?;
                Ok(())
            }

            TokenType::Enum => {
                set_specifier_token(lexer, &mut enum_t, false)?;
                specifiers.type_specifier = parse_enum_specifier(lexer, arena)?;
                Ok(())
            }

            _ => {
                // TODO: Handle user defined types
                return Err(CompilationError::UnexpectedToken(cur_token));
            }
        }
    };

    // We require at least one declaration specifier
    parse_declaration_specifier(lexer.into(), arena)?;

    while let Ok(res) = parse_declaration_specifier(lexer, arena) {
        println!("In here {res:?}");
    }

    // Ensure that no invalid combination of storage specifiers occurs.
    match (static_token, extern_token, typedef_token) {
        (Some(_), None, None) => specifiers.is_static = true,
        (None, Some(_), None) => specifiers.is_extern = true,
        (None, None, Some(_)) => specifiers.is_typedef = true,
        (Some(s), Some(e), _) | (Some(s), _, Some(e)) => {
            return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                prev_specifier: e,
                specifier: s,
            })
        }
        (_, Some(e), Some(t)) => {
            return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                prev_specifier: e,
                specifier: t,
            })
        }
        (None, None, None) => (),
    }

    // Convert the type specifiers to a single type specifier
    match (
        unsigned_t,
        signed_t,
        char_t,
        short_t,
        int_t,
        long_t,
        float_t,
        double_t,
        void_t,
        enum_t,
        struct_or_union_t,
        type_name_t,
    ) {
        (Some(u), Some(s), ..) => {
            return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                prev_specifier: u,
                specifier: s,
            })
        }

        // Chars (signed is default)
        (None, _, Some(_c), None, None, None, None, None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::SignedChar
        }
        (Some(_u), None, Some(_c), None, None, None, None, None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::UnsignedChar
        }

        // Short
        (None, _, None, Some(_s), None, None, None, None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::Short
        }
        (Some(_u), None, None, Some(_s), None, None, None, None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::UnsignedShort
        }

        // Int.
        (None, _, None, None, Some(_), None, None, None, None, None, None, None)
        | (None, Some(_), None, None, None, None, None, None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::Int
        }
        (Some(_u), None, None, None, _, None, None, None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::UnsignedInt
        }

        // Long
        (None, _, None, None, _, Some(_l), None, None, None, None, None, None) => {
            if long_token_count == 1 {
                specifiers.type_specifier = TypeSpecifier::Long;
            } else {
                specifiers.type_specifier = TypeSpecifier::LongLong;
            }
        }

        (Some(_u), None, None, None, _, Some(_l), None, None, None, None, None, None) => {
            if long_token_count == 1 {
                specifiers.type_specifier = TypeSpecifier::UnsignedLong;
            } else {
                specifiers.type_specifier = TypeSpecifier::UnsignedLongLong;
            }
        }

        (None, None, None, None, None, None, Some(_f), None, None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::Float
        }

        (None, None, None, None, None, None, None, Some(_d), None, None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::Double
        }
        (None, None, None, None, None, Some(l), None, Some(d), None, None, None, None) => {
            if long_token_count == 1 {
                specifiers.type_specifier = TypeSpecifier::LongDouble;
            } else {
                return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                    prev_specifier: l,
                    specifier: d,
                });
            }
        }

        (None, None, None, None, None, None, None, None, Some(_v), None, None, None) => {
            specifiers.type_specifier = TypeSpecifier::Void
        }

        (None, None, None, None, None, None, None, None, None, None, None, Some(t)) => {
            if let TokenType::TypeName(ta) = t.token_type {
                specifiers.type_specifier = TypeSpecifier::TypeAlias(ta);
                specifiers.is_const |= ta.alias.specifiers.is_const;
                specifiers.is_volatile |= ta.alias.specifiers.is_volatile;
            } else {
                panic!("Internal Compiler Error: Token type should never not resolve to type")
            }
        }

        (Some(s), None, ..) | (None, Some(s), ..) => {
            // Capture all the cases where unsigned/signed should not be used
            // TODO: Unique error for trying to add types to an already type-defed type?
            let tokens = [
                float_t,
                double_t,
                void_t,
                enum_t,
                struct_or_union_t,
                type_name_t,
            ];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: s,
                        specifier: t,
                    });
                }
            }
        }

        // For structs and enums the type specifier has already been set.
        (.., Some(_), None, None) | (.., None, Some(_), None) => (),

        (None, None, Some(c), ..) => {
            // Capture all the cases where char should not be used
            let tokens = [
                short_t,
                int_t,
                long_t,
                float_t,
                double_t,
                void_t,
                enum_t,
                struct_or_union_t,
                type_name_t,
            ];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: c,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, Some(s), ..) => {
            // Capture all the cases where short should not be used
            let tokens = [
                long_t,
                float_t,
                double_t,
                void_t,
                enum_t,
                struct_or_union_t,
                type_name_t,
            ];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: s,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, None, Some(i), ..) => {
            // Capture all the cases where int should not be used
            let tokens = [
                float_t,
                double_t,
                void_t,
                enum_t,
                struct_or_union_t,
                type_name_t,
            ];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: i,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, None, None, Some(l), ..) => {
            // Capture all the cases where long should not be used
            let tokens = [
                float_t,
                double_t,
                void_t,
                enum_t,
                struct_or_union_t,
                type_name_t,
            ];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: l,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, None, None, None, Some(f), ..) => {
            // Capture all the cases where float should not be used
            let tokens = [double_t, void_t, enum_t, struct_or_union_t, type_name_t];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: f,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, None, None, None, None, Some(d), ..) => {
            // Capture all the cases where double should not be used
            let tokens = [void_t, enum_t, struct_or_union_t, type_name_t];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: d,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, None, None, None, None, None, Some(v), ..) => {
            // Capture all the cases where void should not be used
            let tokens = [enum_t, struct_or_union_t, type_name_t];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: v,
                        specifier: t,
                    });
                }
            }
        }
        (None, None, None, None, None, None, None, None, None, Some(e), ..) => {
            // Capture all the cases where enum should not be used
            let tokens = [struct_or_union_t, type_name_t];
            for t in tokens {
                if let Some(t) = t {
                    return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                        prev_specifier: e,
                        specifier: t,
                    });
                }
            }
        }

        (None, None, None, None, None, None, None, None, None, None, Some(s), Some(t)) => {
            return Err(CompilationError::CannotCombineDeclarationSpecifiers {
                prev_specifier: s,
                specifier: t,
            })
        }

        (None, None, None, None, None, None, None, None, None, None, None, None) => {
            panic!("Not covered")
        }
    }

    let specifiers_node = arena.push(specifiers)?;
    Ok(specifiers_node)
}

// parameter_declaration
// 	: declaration_specifiers declarator
// 	| declaration_specifiers abstract_declarator
// 	| declaration_specifiers
// 	;
fn parse_parameter_declaration<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena ParamDeclaration<'arena>, CompilationError<'arena>> {
    let declaration_specifiers = parse_declaration_specifiers(lexer, arena, true)?;

    // A parameter_declaration is only ever part of a parameter_list, which in turn is only ever part of a parameter_type_list.
    // As such, we know for certain that a parameter_declaration does not have a declarator if the next token is a "," (as this
    // indicates another following parameter_declaration) or ")" (as this marks the end of a parameter_type_list)
    let declarator = match peek_token(lexer)?.token_type {
        TokenType::Comma | TokenType::RParen => None,
        _ => {
            let declarator = parse_declarator(lexer, arena, declaration_specifiers, true)?;
            Some(declarator)
        }
    };

    let param_declaration = ParamDeclaration {
        declaration_specifiers,
        declarator,
    };
    return Ok(arena.push(param_declaration)?);
}

// parameter_type_list
// 	: parameter_list
// 	| parameter_list ',' ELLIPSIS

// parameter_list
// 	: parameter_declaration
// 	| parameter_list ',' parameter_declaration
fn parse_parameter_type_list<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena ParameterTypeList<'arena>, CompilationError<'arena>> {
    // TODO: Implement parsing of ellipsis for variadic functions.

    let param_declaration = parse_parameter_declaration(lexer, arena)?;

    let mut list = vec![param_declaration];

    while let TokenType::Comma = peek_token(lexer)?.token_type {
        next_token(lexer)?;

        list.push(parse_parameter_declaration(lexer, arena)?);
    }

    let slice = arena.push_slice_copy(&list[..])?;
    let param_type_list = ParameterTypeList {
        param_declarations: slice,
        has_ellipsis: false,
    };
    return Ok(arena.push(param_type_list)?);
}

// abstract_declarator
// 	: pointer
// 	| direct_abstract_declarator
// 	| pointer direct_abstract_declarator
// 	;

// direct_abstract_declarator
// 	: '(' abstract_declarator ')'
// 	| '[' ']'
// 	| '[' constant_expression ']'
// 	| direct_abstract_declarator '[' ']'
// 	| direct_abstract_declarator '[' constant_expression ']'
// 	| '(' ')'
// 	| '(' parameter_type_list ')'
// 	| direct_abstract_declarator '(' ')'
// 	| direct_abstract_declarator '(' parameter_type_list ')'
// 	;

// an abstract declarator is just a declarator without an identifier

//pointer
//	: '*'
//	| '*' type_qualifier_list
//	| '*' pointer
//	| '*' type_qualifier_list pointer

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
// 	| direct_declarator '(' identifier_list ')' NOTE: Only needed for K&R function definitions
// 	| direct_declarator '(' ')'

// declarator
// 	: pointer direct_declarator
// 	| direct_declarator
fn parse_declarator_recursive<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    can_be_abstract: bool,
    derived_types: &mut Vec<DerivedType<'arena>>,
    name: &mut Option<&'arena str>,
) -> Result<(), CompilationError<'arena>> {
    // parse pointer derived types but don't push them to list of derived types on the way down
    // the declarator tree
    let mut pointer_derived_types = Vec::<DerivedType>::new();
    loop {
        let peek_tok = peek_token(lexer)?;
        match peek_tok.token_type {
            TokenType::Const => {
                let p = pointer_derived_types
                    .last_mut()
                    .ok_or(CompilationError::UnexpectedToken(peek_tok))?;

                if let DerivedType::Pointer { is_volatile, .. } = *p {
                    *p = DerivedType::Pointer {
                        is_const: true,
                        is_volatile,
                    };
                }
            }

            TokenType::Volatile => {
                let p = pointer_derived_types
                    .last_mut()
                    .ok_or(CompilationError::UnexpectedToken(peek_tok))?;

                if let DerivedType::Pointer { is_const, .. } = *p {
                    *p = DerivedType::Pointer {
                        is_const,
                        is_volatile: true,
                    };
                }
            }

            TokenType::Asterisk => {
                pointer_derived_types.push(DerivedType::Pointer {
                    is_const: false,
                    is_volatile: false,
                });
            }
            _ => break,
        }
        next_token(lexer)?;
    }

    // If the declarator can be abstract, it will not neccessarily have an identifier as a base case.
    // We need to be more careful: A Lparen could either indicate a function call, a parameter type list
    // or another abstract declarator.
    if can_be_abstract {
        let cur_token = peek_token(lexer)?;
        match cur_token.token_type {
            TokenType::Identifier(id_name) => {
                next_token(lexer)?;
                *name = Some(id_name);
            }
            TokenType::LBracket => (),
            TokenType::LParen => {
                // For an abstract declarator, a left-paren may indicate a function call derived type,
                // a parameter type list (function arguments) or the beginnning of another abstract declarator.
                let cur_token = next_token(lexer)?;
                match peek_token(lexer)?.token_type {
                    TokenType::RParen => {
                        // Function call derived type. Add it and continue
                        next_token(lexer)?;
                        derived_types.push(DerivedType::FunctionParameterless);
                    }
                    TokenType::Identifier(_)
                    | TokenType::LBracket
                    | TokenType::LParen
                    | TokenType::Asterisk => {
                        // An abstract declarator can only start with the above tokens,
                        // and a parameter type list can not start with any one of them.
                        // Hence we must be dealing with an abstract declarator.

                        let decl_res = parse_declarator_recursive(
                            lexer,
                            arena,
                            can_be_abstract,
                            derived_types,
                            name,
                        );
                        expect_closing_delimiter(lexer, cur_token, TokenType::RParen)?;
                        decl_res?
                    }

                    _ => {
                        // The remaining possibility is a parameter type list
                        let param_list_res = parse_parameter_type_list(lexer, arena);
                        expect_closing_delimiter(lexer, cur_token, TokenType::RParen)?;
                        let parameter_type_list = param_list_res?;
                        derived_types.push(DerivedType::Function {
                            parameter_type_list,
                        });
                    }
                }
            }
            _ => return Err(CompilationError::UnexpectedToken(cur_token)),
        }
    } else {
        // Not a possibility of declarator not having an identifier as a base-case.
        // Parse either an identifier (base case) or another parenthesized
        // declarator (recursive case).
        let cur_token = next_token(lexer)?;
        match cur_token.token_type {
            TokenType::Identifier(id_name) => {
                *name = Some(id_name);
            }
            TokenType::LParen => {
                let decl_res =
                    parse_declarator_recursive(lexer, arena, can_be_abstract, derived_types, name);
                expect_closing_delimiter(lexer, cur_token, TokenType::RParen)?;
                decl_res?
            }
            _ => return Err(CompilationError::UnexpectedToken(cur_token)),
        }
    }

    // Each "layer" of a declarator can only contain one derived type:
    // 1. [Base] is an array of ...
    // 2. [Base] is a function returning...
    // The former of the two can be repeated more than once in a declarator layer,
    // the latter cannot.
    // TODO: Ensure a function derived type is not followed by another function derived type.

    loop {
        match peek_token(lexer)?.token_type {
            TokenType::LParen => {
                // function declaration
                let l_paren = next_token(lexer)?;

                if let TokenType::RParen = peek_token(lexer)?.token_type {
                    next_token(lexer)?;
                    derived_types.push(DerivedType::FunctionParameterless);
                } else {
                    let param_list_res = parse_parameter_type_list(lexer, arena);
                    expect_closing_delimiter(lexer, l_paren, TokenType::RParen)?;
                    let parameter_type_list = param_list_res?;
                    derived_types.push(DerivedType::Function {
                        parameter_type_list,
                    });
                }
            }
            TokenType::LBracket => {
                next_token(lexer)?;
                if let TokenType::RBracket = peek_token(lexer)?.token_type {
                    next_token(lexer)?;
                    derived_types.push(DerivedType::Array { size: None });
                } else {
                    let constant_expression = parse_expr(lexer, arena, 0, &[TokenType::RBracket])?;
                    let size = eval_const_expression(constant_expression)?;
                    derived_types.push(DerivedType::Array { size: Some(size) });
                    expect_token(lexer, TokenType::RBracket)?;
                }
            }
            _ => break,
        }
    }

    // ... finally push the prefix derived types which were parsed on the way down
    derived_types.append(&mut pointer_derived_types);

    return Ok(());
}

fn parse_declarator<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    specifiers: &'arena Specifiers,
    can_be_abstract: bool,
) -> Result<&'arena DeclaratorNode<'arena>, CompilationError<'arena>> {
    let mut name = None;

    // If the specifiers contained a typedef'ed type, we need to take
    // into account any derived types it may have.
    let mut derived_types = match specifiers.type_specifier {
        TypeSpecifier::TypeAlias(type_alias) => type_alias.alias.derived_types.to_vec(),
        _ => Vec::new(),
    };

    parse_declarator_recursive(lexer, arena, can_be_abstract, &mut derived_types, &mut name)?;
    //println!("Declarator {:?}, {:?}", name, derived_types_list);

    let derived_types = arena.push_slice_copy(&derived_types[..])?;
    let declarator = arena.push(DeclaratorNode {
        name,
        derived_types,
    })?;
    return Ok(declarator);
}

//initializer_list
//	: initializer
//	| initializer_list ',' initializer
//	;
fn parse_initializer_list<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let initializer = parse_initializer(lexer, arena)?;

    let mut list = vec![initializer];

    while let TokenType::Comma = peek_token(lexer)?.token_type {
        next_token(lexer)?;
        // An initializer_list can end with a trailing comma.
        if let TokenType::LCurlyBracket = peek_token(lexer)?.token_type {
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
fn parse_initializer<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    match peek_token(lexer)?.token_type {
        TokenType::LCurlyBracket => {
            next_token(lexer)?;
            let initializer_list = parse_initializer_list(lexer, arena)?;
            expect_token(lexer, TokenType::RCurlyBracket)?;
            return Ok(initializer_list);
        }

        _ => {
            return parse_expr(
                lexer,
                arena,
                0,
                &[TokenType::Comma, TokenType::RCurlyBracket],
            )
        }
    }
}

//init_declarator
//	: declarator
//	| declarator '=' initializer
//	;
fn parse_init_declarator<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    specifiers: &'arena Specifiers,
) -> Result<&'arena InitDeclaratorNode<'arena>, CompilationError<'arena>> {
    let declarator = parse_declarator(lexer, arena, specifiers, false)?;
    match peek_token(lexer)?.token_type {
        TokenType::Assign => {
            next_token(lexer)?;
            let initializer = parse_initializer(lexer, arena)?;
            let init_declarator = InitDeclaratorNode {
                declarator,
                initializer,
            };
            return Ok(arena.push(init_declarator)?);
        }

        _ => {
            let init_declarator = InitDeclaratorNode {
                declarator,
                initializer: arena.push(Node::Empty)?,
            };
            return Ok(arena.push(init_declarator)?);
        }
    }
}

//init_declarator_list
//	: init_declarator
//	| init_declarator_list ',' init_declarator
fn parse_init_declarator_list<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    specifiers: &'arena Specifiers,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    // TODO: Implement init_declarator_list
    let init_declarator = parse_init_declarator(lexer, arena, specifiers)?;
    let mut list = vec![init_declarator];
    while let TokenType::Comma = peek_token(lexer)?.token_type {
        next_token(lexer)?;
        list.push(parse_init_declarator(lexer, arena, specifiers)?);
    }

    let list = arena.push_slice_copy(&list[..])?;
    let node = arena.push(Node::InitDeclaratorList(list))?;
    return Ok(node);
}

// declaration
// 	: declaration_specifiers ';' // This must be allowed for struct declarations
// 	| declaration_specifiers init_declarator_list ';'
// 	;
// e.g. void; int a = 3;
pub fn parse_declaration<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
    consume_semicolon: bool,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let declaration_specifiers = parse_declaration_specifiers(lexer, arena, true)?;
    let init_declarator_list: &Node;

    if TokenType::Semicolon == peek_token(lexer)?.token_type {
        init_declarator_list = arena.push(Node::Empty)?;
    } else {
        init_declarator_list = parse_init_declarator_list(lexer, arena, declaration_specifiers)?;

        if declaration_specifiers.is_typedef {
            if let Node::InitDeclaratorList(init_decl_list) = *init_declarator_list {
                for init_decl in init_decl_list {
                    // A typedef expression can not have an initializer
                    // TODO: We would like an error which can refer to a token.
                    // Solution might be to add a "main" token to each node,
                    // which is the token that most closely represents that node.
                    match init_decl.initializer {
                        Node::Empty => (),
                        _ => {
                            return Err(CompilationError::IllegalInitializer {
                                identifier_name: init_decl.declarator.name.unwrap(),
                            })
                        }
                    }

                    let derived_types = init_decl.declarator.derived_types;
                    let typedef_type = Type {
                        specifiers: declaration_specifiers,
                        derived_types,
                    };
                    let alias = TypeAlias {
                        name: init_decl.declarator.name.unwrap(),
                        alias: typedef_type,
                    };

                    lexer.type_aliases.push(arena.push(alias)?);
                    println!("{:?}", lexer.type_aliases);
                }
            }
        }
    }

    if consume_semicolon {
        expect_token(lexer, TokenType::Semicolon)?;
    }

    let declaration = arena.push(Node::Declaration {
        declaration_specifiers,
        init_declarator_list,
    })?;
    return Ok(declaration);
}

//declaration_list
//	: declaration
//	| declaration_list declaration
//	;

pub fn parse_expr_statement<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    coz::scope!("parse_expr_statement");
    match peek_token(lexer)?.token_type {
        TokenType::Semicolon => {
            next_token(lexer)?;
            return Ok(arena.push(Node::Empty)?);
        }
        _ => {
            let expr = parse_expr(lexer, arena, 0, &[TokenType::Semicolon])?;
            expect_token(lexer, TokenType::Semicolon)?;
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
pub fn parse_statement<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let peek = peek_token(lexer)?;

    let parse_delimeted_expr =
        |lexer: &mut Lexer<'arena>, arena: &'arena Arena| -> ParseResult<'arena> {
            let opening_delim = expect_token(lexer, TokenType::LParen)?;
            let condition_res = parse_expr(lexer, arena, 0, &[TokenType::RParen]);
            expect_closing_delimiter(lexer, opening_delim, TokenType::RParen)?;
            condition_res
        };

    match peek.token_type {
        //selection_statement
        //	: IF '(' expression ')' statement
        //	| IF '(' expression ')' statement ELSE statement
        //	| SWITCH '(' expression ')' statement
        TokenType::If => {
            let mut if_vec = Vec::<&IfNode>::new();

            // Rather than parsing if statements recursively,
            // we attempt to parse all if statements on the same "level"
            // into a single list which can be iterated over. This should
            // make SSA/codegen easier.
            loop {
                next_token(lexer)?; // consume the if token

                // Consume the if-expression and its delimeters
                let condition = parse_delimeted_expr(lexer, arena)?;

                let statement = parse_statement(lexer, arena)?;
                let if_node = arena.push(IfNode {
                    condition,
                    statement,
                })?;
                if_vec.push(if_node);

                if let TokenType::Else = peek_token(lexer)?.token_type {
                    next_token(lexer)?;

                    if let TokenType::If = peek_token(lexer)?.token_type {
                        //
                        continue;
                    } else {
                        // Just an else with no condition: this ends the series of
                        // if-else statements
                        let else_statement = parse_statement(lexer, arena)?;
                        let else_condition = arena.push(Node::Empty)?;
                        let else_node = arena.push(IfNode {
                            condition: else_condition,
                            statement: else_statement,
                        })?;
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
        TokenType::Continue | TokenType::Break => {
            return Err(CompilationError::NotImplemented);
            //let node = Node::Terminal(*peek_token);
            //expect_token(lexer, Token::Semicolon);
            //return Ok(NodePtr::new(node));
        }

        TokenType::Goto => {
            next_token(lexer)?;
            // TODO: A lot of these .ok_or() could be a lot more succint with a function
            let possible_id_token = next_token(lexer)?;
            match possible_id_token.token_type {
                TokenType::Identifier(name) => {
                    let node = Node::Goto { goto_id: name };
                    expect_token(lexer, TokenType::Semicolon)?;
                    return Ok(arena.push(node)?);
                }
                _ => return Err(CompilationError::UnexpectedToken(possible_id_token)),
            }
        }

        TokenType::Return => {
            next_token(lexer)?;
            match peek_token(lexer)?.token_type {
                TokenType::Semicolon => {
                    next_token(lexer)?;
                    let node = Node::Return {
                        expression: arena.push(Node::Empty)?,
                    };
                    return Ok(arena.push(node)?);
                }

                _ => {
                    let expression = parse_expr(lexer, arena, 0, &[TokenType::Semicolon])?;
                    expect_token(lexer, TokenType::Semicolon)?;
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
        TokenType::While => {
            next_token(lexer)?;

            let condition = parse_delimeted_expr(lexer, arena)?;

            let statement = parse_statement(lexer, arena)?;
            let node = Node::WhileStatement {
                condition,
                statement,
            };
            return Ok(arena.push(node)?);
        }

        TokenType::Do => {
            next_token(lexer)?;
            let statement = parse_statement(lexer, arena)?;

            expect_token(lexer, TokenType::While)?;
            let condition = parse_delimeted_expr(lexer, arena)?;
            expect_token(lexer, TokenType::Semicolon)?;

            let node = Node::DoWhileStatement {
                condition,
                statement,
            };
            return Ok(arena.push(node)?);
        }

        TokenType::For => {
            next_token(lexer)?;
            let l_paren = expect_token(lexer, TokenType::LParen)?;

            let mut parse_for_header =
                || -> Result<(&Node, &Node, &Node), CompilationError<'arena>> {
                    let init_statement = parse_expr_statement(lexer, arena)?;
                    let condition = parse_expr_statement(lexer, arena)?;
                    let iter_statement = if let TokenType::RParen = peek_token(lexer)?.token_type {
                        parse_expr(lexer, arena, 0, &[TokenType::RParen])?
                    } else {
                        arena.push(Node::Empty)?
                    };
                    Ok((init_statement, condition, iter_statement))
                };
            let parse_for_header_res = parse_for_header();
            expect_closing_delimiter(lexer, l_paren, TokenType::RParen)?;
            let (init_statement, condition, iter_statement) = parse_for_header_res?;

            let statement = parse_statement(lexer, arena)?;

            let node = Node::ForStatement {
                init_statement,
                condition,
                iter_statement,
                statement,
            };
            return Ok(arena.push(node)?);
        }

        TokenType::LCurlyBracket => return parse_compound_statement(lexer, arena),

        _ => {
            // No applicable keywords encountered, try parsing expression
            return parse_expr_statement(lexer, arena);
        }
    }
}

//compound_statement
//	: '{' '}'
//	| '{' statement_list '}'
//	| '{' declaration_list '}'
//	| '{' declaration_list statement_list '}'
//	;
fn parse_compound_statement<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    coz::scope!("compound_statement");
    let l_curl = expect_token(lexer, TokenType::LCurlyBracket)?;

    let mut parse_compound_contents = || -> Result<&Node, CompilationError> {
        let mut declarations = Vec::<&Node>::new();

        loop {
            match peek_token(lexer)?.token_type {
                DECLARATION_SPECIFIER_STARTER!() => {
                    let declaration = parse_declaration(lexer, arena, true)?;
                    declarations.push(declaration);
                }
                _ => break,
            }
            /*
            match parse_declaration(lexer, arena, true) {
                Ok(node) => declarations.push(node),
                Err(CompilationError::UnexpectedToken(Token { token_type : TokenType::Int, ..})) => {
                    break;
                }
                Err(e) => return Err(e),
            }*/
        }

        // statement_list
        // 	: statement
        // 	| statement_list statement
        // 	;
        let mut statements = Vec::<&Node>::new();
        loop {
            match peek_token(lexer)?.token_type {
                TokenType::RCurlyBracket => {
                    // println!("found rcurl, stopping...");
                    break;
                }
                _ => {
                    let statement = parse_statement(lexer, arena)?;
                    statements.push(statement);
                }
            }
        }

        let statements = arena.push_slice_copy(&statements[..])?;
        let statement_list = arena.push(Node::StatementList(statements))?;

        let declarations = arena.push_slice_copy(&declarations[..])?;
        let declaration_list = arena.push(Node::DeclarationList(declarations))?;
        let compound_statement = Node::CompoundStatement {
            declaration_list,
            statement_list,
        };
        Ok(arena.push(compound_statement)?)
    };

    let contents_res = parse_compound_contents();
    expect_closing_delimiter(lexer, l_curl, TokenType::RCurlyBracket)?;
    Ok(contents_res?)
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
fn parse_external_declaration<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    let declaration = parse_declaration(lexer, arena, false)?;

    let peek_token = peek_token(lexer)?;
    match peek_token.token_type {
        TokenType::Semicolon => {
            // Semicolon encountered - this external declaration must be a declaration.
            next_token(lexer)?;
            return Ok(declaration);
        }

        TokenType::LCurlyBracket => {
            // Left curly bracket - this external declaration must be a function_definition
            // we must now "extract" the declaration_specifiers and declarator from
            // the previously parsed declaration
            let func_declaration_specifiers: &Specifiers;
            let func_declarator: &DeclaratorNode;

            // TODO: Safe to just unwrap here
            if let Node::Declaration {
                declaration_specifiers,
                init_declarator_list,
            } = *declaration
            {
                func_declaration_specifiers = declaration_specifiers;

                if let Node::InitDeclaratorList(init_list) = *init_declarator_list {
                    if init_list.len() != 1 {
                        return Err(CompilationError::UnableToDecomposeDeclaration);
                    }
                    let init_node = init_list[0];
                    let first_type = init_node
                        .declarator
                        .derived_types
                        .last()
                        .ok_or(CompilationError::UnableToDecomposeDeclaration)?;
                    match first_type {
                        DerivedType::Function { .. } | DerivedType::FunctionParameterless => {
                            func_declarator = init_node.declarator
                        }
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
            let func_definition = Node::FunctionDefinition {
                declaration_specifiers: func_declaration_specifiers,
                declarator: func_declarator,
                compound_statement,
            };
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
pub fn parse_translational_unit<'arena>(
    lexer: &mut Lexer<'arena>,
    arena: &'arena Arena,
) -> Result<&'arena Node<'arena>, CompilationError<'arena>> {
    // a translation_unit must have at least one external_declaration according to the grammar
    let first_declaration = parse_external_declaration(lexer, arena)?;
    let mut external_declarations = vec![first_declaration];

    while lexer.peek_token().is_some() {
        let declaration = parse_external_declaration(lexer, arena)?;
        external_declarations.push(declaration);
    }

    let external_declarations = arena.push_slice_copy(&external_declarations[..])?;
    let translation_unit = Node::TranslationalUnit {
        external_declarations,
    };
    return Ok(arena.push(translation_unit)?);
}
