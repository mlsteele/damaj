package compile
import util.CLI
import scala.util.parsing.input.Reader
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.PagedSeq
import java.io._
import scala.io.Source
import scala.collection.mutable.{StringBuilder, ListBuffer}
import scala.Console

import org.antlr.runtime.tree.ParseTree
import org.antlr.runtime.debug.ParseTreeBuilder
import org.antlr.runtime.{ANTLRFileStream, Token, CommonTokenStream}
import grammars.{DecafScanner, DecafParser}

object Compiler {
  val tokenMap: Map[Int, String] = Map(
    DecafScanner.IDENTIFIER -> "IDENTIFIER",
    DecafScanner.CHARLITERAL -> "CHARLITERAL",
    DecafScanner.INTLITERAL -> "INTLITERAL",
    DecafScanner.BOOL_LITERAL -> "BOOLEANLITERAL"
  )

  var outFile = if (CLI.outfile == null) Console.out else (new java.io.PrintStream(
    new java.io.FileOutputStream(CLI.outfile)))

  def main(args: Array[String]): Unit = {
    CLI.parse(args, Array[String]());
    if (CLI.target == CLI.Action.SCAN) {
      scan(CLI.infile)
      System.exit(0)
    } else if (CLI.target == CLI.Action.PARSE) {
      val tree = parse(CLI.infile)
      tree match {
        case Some(tree) => System.exit(0)
        case None => System.exit(1)
      }
    }
  }

  def scan(fileName: String) {
    try {
      val inputStream: ANTLRFileStream = new ANTLRFileStream(fileName)
      val scanner = new DecafScanner(inputStream)
      // scanner.setTrace(CLI.debug)
      var done = false
      while (!done) {
        try {
          val head = scanner.nextToken()
          if (head.getChannel() != Token.HIDDEN_CHANNEL) {
            if (head.getType() == DecafScanner.EOF) {
              done = true
            } else {
              val tokenType = head.getType();
              val tokenTypeName: String = tokenMap.getOrElse(tokenType, "")
              outFile.println(
                head.getLine()
                + (if (tokenTypeName ==  "") "" else " ")
                // + " (" + head.getType() + ") "
                + tokenTypeName
                + " "
                + head.getText())
            }
          }
        } catch {
          case ex: Exception => {
            Console.err.println(CLI.infile + " " + ex)
            // scanner.consume();
          }
        }
      }
    } catch {
      case ex: Exception => Console.err.println(ex)
    }
  }

  def parse(fileName: String): Option[ParseTree] = {
    val inputStream: ANTLRFileStream = new ANTLRFileStream(fileName)
    val scanner = new DecafScanner(inputStream)
    val tokens = new CommonTokenStream(scanner)
    val treebuilder = new ParseTreeBuilder("program")
    val parser = new DecafParser(tokens, treebuilder)
    parser.program()
    val tree = Option(treebuilder.getTree()) match {
      case Some(tree) => Some(tree.getChild(0).asInstanceOf[ParseTree])
      case None => None
    }
    val error = parser.getNumberOfSyntaxErrors() > 0

    // Error reporting
    (error, tree) match {
      case (true, None) =>
        Console.err.println("[ERROR]: Parse error")
      case (true, Some(tree)) =>
        Console.err.println("[ERROR]: Parse error but tree exists")
      case (false, None) =>
        Console.err.println("[ERROR]: No parse error but no parse tree")
      case (false, Some(tree)) =>
        Console.err.println("[YAY]: Parse succeeded")
    }

    // Debug print
    tree match {
      case Some(tree) =>
        print_tree(tree, 0)
        Console.err.println("HERE COMES THE AST")
        val ast = ASTTools.parseNode(tree)
        Console.err.println(ast)
        // Console.err.println(tree.toStringTree())
        // Console.err.println(tree.getChild(0).toStringTree())
        // Console.err.println(tree.getChild(0).getChild(0).getChild(0).toStringTree())
        // Console.err.println(tree.getChild(0).getChild(0).getType())
        // Console.err.println(tree.getChild(0).getChild(0).toString())
        // Console.err.println(tree.getChild(0).getChild(0).getChild(1).getText())
      case _ =>
    }

    // return value
    (error, tree) match {
      case (false, Some(tree)) => Some(tree)
      case _ => None
    }
  }

  // def parse(fileName: String): Option[CommonAST]  = {
    // var inputStream : java.io.FileInputStream = null
    // try {
      // inputStream = new java.io.FileInputStream(fileName)
    // } catch {
      // case f: FileNotFoundException =>
        // Console.err.println("File " + fileName + " does not exist")
        // return None
    // }

    // val scanner = new DecafScanner(new DataInputStream(inputStream))
    // val parser = new DecafParser(scanner);
    // parser.setTrace(CLI.debug)
    // parser.program()
    // val tree = Option(parser.getAST().asInstanceOf[CommonAST])
    // val error: Boolean = parser.getError()

    // // error reporting
    // (error, tree) match {
      // case (true, None) =>
        // Console.err.println("[ERROR]: Parse error\n")
      // case (true, Some(tree)) =>
        // Console.err.println("[ERROR]: Parse error with tree\n")
      // case (false, None) =>
        // Console.err.println("[ERROR] No parse tree but no error\n")
      // case (false, Some(tree)) =>
        // Console.err.println("[YAY] Parse succeeded\n")
    // }

    // // debug print
    // tree match {
      // case Some(tree) =>
        // // Console.err.println("==string==")
        // // Console.err.println(tree.toString())
        // // Console.err.println("==list==")
        // // Console.err.println(tree.toStringTree())
        // // Console.err.println("==tree==")
        // // Console.err.println(tree.toStringTree())
        // // Console.err.println("==custom-tree==")
        // // print_tree(tree, 0)
        // // Console.err.println("==end==")
      // case _ =>
    // }

    // // return value
    // (error, tree) match {
      // case (false, Some(tree)) => Some(tree)
      // case _ => None
    // }
  // }

  def print_tree(tree: ParseTree, level: Int): Unit  = {
    val prefix = "  " * level
    val ttype = tree.getType()
    val token = tree.getText()
    val children = Range(0, tree.getChildCount()).map (tree.getChild(_).asInstanceOf[ParseTree])
    Console.err.println(prefix + "| " + token + " (" + ttype + ")")
    children.foreach(print_tree(_, level + 1))
  }
}
