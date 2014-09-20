lexer grammar DecafScanner;

@header {
package edu.mit.compilers.grammar;
}

LCURLY : '{' ;
RCURLY : '}' ;
LSQUARE : '[' ;
RSQUARE : ']' ;
LPAREN : '(' ;
RPAREN : ')' ;
COMMA : ',' ;
COLON : ':' ;
SEMICOLON : ' ;';
QUESTION : '?' ;
AT : '@' ;

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

// Note that here, the {} syntax allows you to literally command the lexer
// to skip mark this token as skipped, or to advance to the next line
// by directly adding Java commands.
// WS_ : (' ' | '\t' | '\n' {newline();}) {_ttype = Token.SKIP; };
// SL_COMMENT : "//" (~'\n')* '\n' {_ttype = Token.SKIP; newline (); };

CHARLITERAL : '\'' CHAR '\'' ;
STRINGLITERAL : '"' (CHAR)* '"' ;
INTLITERAL : DEC_LITERAL | HEX_LITERAL ;
fragment DEC_LITERAL : (DIGIT)+ ;
fragment HEX_LITERAL : '0x' (DIGIT|('a'..'f'|'A'..'F'))+ ;

fragment ALPHANUM : (ALPHA | DIGIT) ;
fragment ALPHA : ('a'..'z' | 'A'..'Z') | '_' ;
fragment DIGIT : ('0'..'9') ;
fragment CHAR : (ESC | ~('\'' | '"' | '\\' | '\t' | '\n' )) ;
fragment ESC : '\\' ('n'|'t'|'"'|'\\'|'\'') ;
