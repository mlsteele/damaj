header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

{@SuppressWarnings("unchecked")}
class DecafScanner extends Lexer;
options
{
  k = 2;
}

tokens {
  "boolean";
  "true";
  "false";
  "callout";
  "else";
  "if";
  "int";
  "return";
  "void";
  "break";
  "continue" ;
  "for" ;
}

// Selectively turns on debug tracing mode.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws CharStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws CharStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}

LCURLY options { paraphrase = "{"; } : "{";
RCURLY options { paraphrase = "}"; } : "}";
LSQUARE options { paraphrase = "["; } : "[";
RSQUARE options { paraphrase = "]"; } : "]";
LPAREN options { paraphrase = "("; } : "(";
RPAREN options { paraphrase = ")"; } : ")";
COMMA options { paraphrase = ","; } : ",";
SEMICOLON options { paraphrase = ";"; } : ";";

// Literals are pulled from 'tokens' above.
IDENTIFIER options { testLiterals=true; } : ALPHA (ALPHANUM)* ;

// This is one giant rule to avoid lexical nondeterminism warnings.
OP : '?'
   | '!'
   | '+'
   | '-'
   | '*'
   | '/'
   | '%' 
   | '<'
   | '>'
   | '='
   | "<="
   | ">="
   | "=="
   | "!="
   | "+="
   | "-="
   | "&&"
   | "||" ;

// OP : BIN_OP | ASSIGN_OP;
// protected BIN_OP : ARITH_OP | REL_OP | EQ_OP | COND_OP ;
// protected ARITH_OP : '+' | '-' | '*' | '/' | '%' ;
// protected REL_OP : '<' | '>' | "<=" | ">=" ;
// protected EQ_OP : "==" | "!=" ;
// protected COND_OP : "&&" | "||" ;
// protected ASSIGN_OP : '=' | "+=" | "-=" ;

// Note that here, the {} syntax allows you to literally command the lexer
// to skip mark this token as skipped, or to advance to the next line
// by directly adding Java commands.
WS_ : (' ' | '\t' | '\n' {newline();}) {_ttype = Token.SKIP; };
SL_COMMENT : "//" (~'\n')* '\n' {_ttype = Token.SKIP; newline (); };

CHARLITERAL : '\'' CHAR '\'' ;
STRINGLITERAL : '"' (CHAR)* '"' ;
INTLITERAL : DEC_LITERAL | HEX_LITERAL ;
protected DEC_LITERAL : (DIGIT)+ ;
protected HEX_LITERAL : "0x" (DIGIT|ALPHA)+ ;

protected ALPHANUM : (ALPHA | DIGIT) ;
protected ALPHA : ('a'..'z' | 'A'..'Z') | '_' ;
protected DIGIT : ('0'..'9') ;
protected CHAR : (ESC | ~('\'' | '"' | '\\' | '\t' | '\n' )) ;
protected ESC : '\\' ('n'|'t'|'"'|'\\'|'\'') ;
