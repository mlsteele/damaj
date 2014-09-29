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
    if (!(new java.io.File(CLI.infile).exists)){
        Console.err.println(CLI.infile + ": No such file or directory")
        System.exit(1)
    }
    if (CLI.target == CLI.Action.SCAN) {
      scan(CLI.infile)
      System.exit(0)
    } else if (CLI.target == CLI.Action.PARSE) {
      val tree = parse(CLI.infile)
      tree match {
        case Some(tree) => System.exit(0)
        case None => System.exit(1)
      }
    } else if (CLI.target == CLI.Action.INTER) {
      val result = inter(CLI.infile)
      System.exit(result)
    }
  }

  def scan(fileName: String) {
    try {
      val inputStream: ANTLRFileStream = new ANTLRFileStream(fileName)
      val scanner = new DecafScanner(inputStream)
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
          case ex: Exception =>
            Console.err.println(CLI.infile + " " + ex)
        }
      }
    } catch {
      case ex: Exception =>
        Console.err.println(CLI.infile + " " + ex)
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
    if (CLI.debug) {
      tree match {
        case Some(tree) =>
          print("\nParse Tree:")
          println(tree.toStringTree())

          println("\nParse Tree (pretty):")
          Console.err.println(PTTools.prettyprint(tree))

        case _ =>
      }
    }

    // return value
    (error, tree) match {
      case (false, Some(tree)) => Some(tree)
      case _ => None
    }
  }

  def inter(fileName: String): Int = {
      val parseTree = parse(fileName)
      parseTree match {
        case None => return 1
        case Some(parseTree) =>

          val ast = new ASTBuilder(parseTree).ast

          if (CLI.debug) {
            println("\nAST:")
            println(ast)

            val ast_pretty = ASTPrinter.printProgram(ast)
            println(ast_pretty)
          }
      }

      // TODO more intermediate format stuff

      return 0
  }

  def print_tree(tree: ParseTree, level: Int): Unit  = {
    val prefix = "  " * level
    val ttype = tree.getType()
    val token = tree.getText()
    val line = tree.getLine()
    val linechar = tree.getCharPositionInLine()
    val children = Range(0, tree.getChildCount()).map (tree.getChild(_).asInstanceOf[ParseTree])
    Console.err.println(prefix + token + " (t:" + ttype + ", l:" + line + ":" + linechar + ")")
    children.foreach(print_tree(_, level + 1))
  }
}
