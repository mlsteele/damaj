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
    System.out.println("YO AN ERROR ex");
    error = true;
  }
  @Override
  public void reportError (String s) {
    // Print the error via some kind of error reporting mechanism.
    System.out.println("YO AN ERROR s");
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

program : (callout_decl)* (field_decl)* (method_decl)* ;
callout_decl : TK_callout id SEMICOLON ;
field_decl : type field_decl_right (COMMA field_decl_right)*  SEMICOLON ;
field_decl_right : (id | id LSQAURE INTLITERAL RSQUARE ) ;
method_decl : (type | TK_void) id LPAREN (type id (COMMA type id)*)? RPAREN block ;
block : LCURLY (field_decl)* (statement)* RCURLY ;
type : INTLITERAL | bool_literal ;
statement : location assign_op expr SEMICOLON
          | method_call SEMICOLON
          | TK_if LPAREN expr RPAREN block (TK_else block)?
          | TK_for LPAREN id SETEQ expr COMMA expr RPAREN block
          | TK_while LPAREN expr RPAREN (COLON INTLITERAL)? block
          | TK_return (expr)? SEMICOLON
          | TK_break SEMICOLON
          | TK_continue SEMICOLON ;
assign_op : SETEQ | SETINCR | SETDECR ;
method_call : method_name LPAREN expr (COMMA expr)* RPAREN
            | method_name LPAREN callout_arg (COMMA callout_arg)* RPAREN ;
method_name : id ;
location : id | id LSQAURE expr RSQUARE ;
// expr : // Oh, brother.
       // expr BINOP expr ;
expr : expr_prefix | expr_norec ;
// TODO: ternary
// expr : expr TERN_START expr TERN_DIVIDER expr ;
expr_prefix :
      MINUS expr
    | BANG expr
    | LPAREN expr RPAREN ;
expr_norec :
      location
    | method_call
    | literal
    | AT id ;
callout_arg : expr | STRINGLITERAL ;
literal : INTLITERAL | CHARLITERAL | bool_literal ;
id : IDENTIFIER ;
bool_literal : TK_true | TK_false ;
