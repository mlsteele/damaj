header {
package edu.mit.compilers.grammar;
}

options
{
  mangleLiteralPrefix = "TK_";
  language = "Java";
}

class DecafParser extends Parser;
options
{
  importVocab = DecafScanner;
  k = 3;
  buildAST = true;
}

// Java glue code that makes error reporting easier.
// You can insert arbitrary Java code into your parser/lexer this way.
{
  // Do our own reporting of errors so the parser can return a non-zero status
  // if any errors are detected.
  /** Reports if any errors were reported during parse. */
  private boolean error;

  @Override
  public void reportError (RecognitionException ex) {
    // Print the error via some kind of error reporting mechanism.
    System.out.print("[ERROR] ");
    System.out.println(ex);
    error = true;
  }
  @Override
  public void reportError (String s) {
    // Print the error via some kind of error reporting mechanism.
    System.out.print("[ERROR] ");
    System.out.println(s);
    error = true;
  }
  public boolean getError () {
    return error;
  }

  // Selectively turns on debug mode.

  /** Whether to display debug information. */
  private boolean trace = false;

  public void setTrace(boolean shouldTrace) {
    trace = shouldTrace;
  }
  @Override
  public void traceIn(String rname) throws TokenStreamException {
    if (trace) {
      super.traceIn(rname);
    }
  }
  @Override
  public void traceOut(String rname) throws TokenStreamException {
    if (trace) {
      super.traceOut(rname);
    }
  }
}

program : (callout_decl)* (field_decl)* (method_decl)* EOF;
callout_decl : TK_callout IDENTIFIER SEMICOLON ;
field_decl : type field_decl_right (COMMA field_decl_right)* SEMICOLON ;
field_decl_right : IDENTIFIER^ (LSQUARE INTLITERAL RSQUARE)? ;
method_decl : (type | TK_void) IDENTIFIER^ LPAREN (type IDENTIFIER (COMMA type IDENTIFIER)*)? RPAREN block ;
block : LCURLY (field_decl)* (statement)* RCURLY ;
type : TK_int | TK_boolean ;
statement : location assign_op expr SEMICOLON
          | method_call SEMICOLON
          | TK_if^ LPAREN expr RPAREN block (TK_else block)?
          | TK_for^ LPAREN IDENTIFIER OP_SET expr COMMA expr RPAREN block
          | TK_while^ LPAREN expr RPAREN (COLON INTLITERAL)? block
          | TK_return^ (expr)? SEMICOLON
          | TK_break^ SEMICOLON
          | TK_continue^ SEMICOLON ;
assign_op : OP_SET^ | OP_INC^ | OP_DEC^ | OP_INV^ ;
method_name : IDENTIFIER ;
expr : eA ;
eA : eB ;
// eA : eB | (LPAREN^ eA RPAREN) ; // parens
eB : eC (QUESTION^ eA COLON eA)? ; // ternary
eC : eD (OP_OR^ eA)? ; // ||
eD : eE (OP_AND^ eA)? ; // &&
eE : eF ((OP_EQ^ | OP_NEQ^) eA)? ; // == !=
eF : eG ((OP_LT^ | OP_GT^ | OP_LTE^ | OP_GTE^) eA)? ; // < <= > >=
eG : eH ((OP_PLUS^ | OP_MINUS^) eA)? ; // + -
eH : eI ((OP_STAR^ | OP_SLASH^ | OP_PERC^) eA)? ; // * /
eI : ((OP_MINUS^ | OP_INV^ | AT) eA) | eZ ; // unary - !
eZ : eJ | (LPAREN eA RPAREN) ; // ()
eJ : location | method_call | literal ;
location : IDENTIFIER^ LSQUARE expr RSQUARE | IDENTIFIER^ ;
method_call : method_name LPAREN (callout_arg (COMMA callout_arg)*)? RPAREN ;
callout_arg : STRINGLITERAL | expr ;
literal : INTLITERAL | CHARLITERAL | bool_literal ;
bool_literal : TK_true | TK_false ;
