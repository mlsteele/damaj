lexer grammar DecafScanner;

@header {
package grammars;
}

// LCURLY : '{' ;
// RCURLY : '}' ;
// LSQUARE : '[' ;
// RSQUARE : ']' ;
// LPAREN : '(' ;
// RPAREN : ')' ;
// COMMA : ',' ;
// COLON : ':' ;
// SEMICOLON : ';' ;
// QUESTION : '?' ;
// AT : '@' ;

// Literals are pulled from 'tokens' above.
IDENTIFIER : ALPHA (ALPHANUM)* ;

// This is one giant rule to avoid lexical nondeterminism warnings.
// They're broken up by 'tokens' above.
OP : '+'
   | '-'
   | '*'
   | '/'
   | '%'
   | '<'
   | '>'
   | '<='
   | '>='
   | '=='
   | '!='
   | '&&'
   | '||'
   | '='
   | '+='
   | '-='
   | '!' ;

// WS_ : (' ' | '\t' | '\n' {newline();}) {_ttype = Token.SKIP; };
// SL_COMMENT : "//" (~'\n')* '\n' {_ttype = Token.SKIP; newline (); };

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
