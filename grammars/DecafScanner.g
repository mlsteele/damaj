lexer grammar DecafScanner;

@header {
package grammars;
}

LCURLY : '{' ;
RCURLY : '}' ;
LSQUARE : '[' ;
RSQUARE : ']' ;
LPAREN : '(' ;
RPAREN : ')' ;
COMMA : ',' ;
COLON : ':' ;
SEMICOLON : ';' ;
QUESTION : '?' ;
AT : '@' ;

KW_boolean : 'boolean' ;
KW_callout : 'callout' ;
KW_else : 'else' ;
KW_if : 'if' ;
KW_int : 'int' ;
KW_return : 'return' ;
KW_void : 'void' ;
KW_break : 'break' ;
KW_continue : 'continue' ;
KW_for : 'for' ;
KW_while : 'while' ;
BOOL_LITERAL : 'true' | 'false' ;
IDENTIFIER : ALPHA (ALPHANUM)*  ;

// Operators
// Arithmetic
OP_PLUS : '+' ;
OP_MINUS : '-' ;
OP_STAR : '*' ;
OP_SLASH : '/' ;
OP_PERC : '%' ;
// Relative
OP_LT : '<' ;
OP_GT : '>' ;
OP_LE : '<=' ;
OP_GE : '>=' ;
// Equality
OP_EQ : '==' ;
OP_NEQ : '!=' ;
// Conditional
OP_AND : '&&' ;
OP_OR : '||' ;
// Assignment Operators
OP_SET : '=' ;
OP_INC : '+=' ;
OP_DEC : '-=' ;
// Other
OP_INV : '!' ;

WHITESPACE : (' ' | '\t' | '\n') {$channel=HIDDEN;};
SL_COMMENT : '/' '/' (~'\n')* '\n' {$channel=HIDDEN;} ;

CHARLITERAL : '\'' CHAR '\'' ;
STRINGLITERAL : '\"' (CHAR)* '\"' ;
INTLITERAL : DEC_LITERAL | HEX_LITERAL ;
fragment DEC_LITERAL : (DIGIT)+ ;
fragment HEX_LITERAL : '0x' (DIGIT|('a'..'f'|'A'..'F'))+ ;

fragment ALPHANUM : (ALPHA | DIGIT) ;
fragment ALPHA : ('a'..'z' | 'A'..'Z') | '_' ;
fragment DIGIT : ('0'..'9') ;
fragment CHAR : (ESC | ~('\'' | '"' | '\\' | '\t' | '\n' )) ;
fragment ESC : '\\' ('n'|'t'|'"'|'\\'|'\'') ;
