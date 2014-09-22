parser grammar DecafParser;

options {
    tokenVocab=DecafScanner;
    output=AST;
}

@header {
package grammars;
}

program : (callout_decl)* (field_decl)* (method_decl)* EOF;
callout_decl : KW_callout IDENTIFIER SEMICOLON ;
field_decl : type field_decl_right (COMMA field_decl_right)* SEMICOLON ;
field_decl_right : IDENTIFIER (LSQUARE INTLITERAL RSQUARE)? ;
method_decl : (type | KW_void) IDENTIFIER LPAREN (type IDENTIFIER (COMMA type IDENTIFIER)*)? RPAREN block ;
block : LCURLY (field_decl)* (statement)* RCURLY ;
type : KW_int | KW_boolean ;
statement : location assign_op expr SEMICOLON
          | method_call SEMICOLON
          | KW_if LPAREN expr RPAREN block (KW_else block)?
          | KW_for LPAREN IDENTIFIER OP_SET expr COMMA expr RPAREN block
          | KW_while LPAREN expr RPAREN (COLON INTLITERAL)? block
          | KW_return (expr)? SEMICOLON
          | KW_break SEMICOLON
          | KW_continue SEMICOLON ;
assign_op : OP_SET | OP_INC | OP_DEC | OP_INV ;
method_name : IDENTIFIER ;
expr : eA ;
eA : eB ;
// eA : eB | (LPAREN eA RPAREN) ; // parens
eB : eC (QUESTION eA COLON eA)? ; // ternary
eC : eD (OP_OR eA)? ; // ||
eD : eE (OP_AND eA)? ; // &&
eE : eF ((OP_EQ | OP_NEQ) eA)? ; // == !=
eF : eG ((OP_LT | OP_GT | OP_LTE | OP_GTE) eA)? ; // < <= > >=
eG : eH ((OP_PLUS | OP_MINUS) eA)? ; // + -
eH : eI ((OP_STAR | OP_SLASH | OP_PERC) eA)? ; // * /
eI : ((OP_MINUS | OP_INV | AT) eA) | eZ ; // unary - !
eZ : eJ | (LPAREN eA RPAREN) ; // ()
eJ : location | method_call | literal ;
location : IDENTIFIER LSQUARE expr RSQUARE | IDENTIFIER ;
method_call : method_name LPAREN (callout_arg (COMMA callout_arg)*)? RPAREN ;
callout_arg : STRINGLITERAL | expr ;
literal : INTLITERAL | CHARLITERAL | BOOL_LITERAL ;
