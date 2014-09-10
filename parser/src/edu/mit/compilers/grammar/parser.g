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
    error = true;
  }
  @Override
  public void reportError (String s) {
    // Print the error via some kind of error reporting mechanism.
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

// TODO: comma separated regexes
program : (callout_decl)* (field_decl)* (method_decl)* ;
callout_decl : callout id ;
field_decl : type field_decl_right (COMMA field_decl_right)* ;
field_decl_right : (id | id LSQAURE int_literal RSQUARE ) ;
method_decl : (type | TK_void) id LPAREN (type id (COMMA type id)*)? RPAREN block ;
block : LCURLY (field_decl)* (statement)* RCURLY ;
type : int | boolean ;
statement : location assign_op expr SEMICOLON
          | method_call SEMICOLON
          | if LPAREN expr RPAREN block (TK_else block)?
          | for LPAREN id '=' expr COMMA expr RPAREN block
          | while LPAREN expr RPAREN (':' int_literal)? block
          | return (expr)? SEMICOLON
          | break SEMICOLON
          | continue SEMICOLON ;
assign_op : '=' | "+=" | "-=" ;
method_call : method_name LPAREN expr (COMMA expr)* RPAREN
            | method_name LPAREN callout_arg (COMMA callout_arg)* RPAREN ;
method_name : id ;
location : id | id LSQAURE expr RSQUARE ;
expr : location
     | method_call
     | literal
     | '@' id
     | expr bin_op expr
     | '-' expr
     | '!' expr
     | LPAREN expr RPAREN
     | expr '?' expr ':' expr ;
callout_arg : expr | string_literal ;
bin_op : arith_op | rel_op | eq_op | cond_op ;
arith_op : '+' | '-' | '*' | '/' | '%' ;
rel_op : '<' | '>' | "<=" | ">=" ;
eq_op : "==" | "!=" ;
cond_op : "&&" | "||" ;
literal : INTLITERAL | CHARLITERAL | bool_literal ;
id : alpha (alpha_num)* ;
bool_literal : TK_true | TK_false ;
