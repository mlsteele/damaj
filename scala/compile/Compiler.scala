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

  var outFile = Console.out

  def main(args: Array[String]): Unit = {
    CLI.parse(args, Array[String]());
    outFile = Option(CLI.outfile) match {
      case None => Console.out
      case Some(file) => new java.io.PrintStream(new java.io.FileOutputStream(file))
    }
    if (CLI.infile == null || !(new java.io.File(CLI.infile).exists)){
        Console.err.println(CLI.infile + ": No such file or directory")
        System.exit(1)
    }
    CLI.target match {
      case CLI.Action.SCAN =>
        scan(CLI.infile)
        System.exit(0)
      case CLI.Action.PARSE =>
        val tree = parse(CLI.infile)
        tree match {
          case Some(tree) => System.exit(0)
          case None => System.exit(1)
        }
      case CLI.Action.INTER =>
        inter(CLI.infile) match {
          case Some(_) => System.exit(0)
          case None    => System.exit(1)
        }
      case CLI.Action.ASSEMBLY | CLI.Action.DEFAULT => {
        val worked = assembly(CLI.infile)
        worked match {
          case true => System.exit(0)
          case false => System.exit(-1)
        }
      }
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
        // Parse succeeded!
        if (CLI.debug && (CLI.target == CLI.Action.PARSE)) {
          // print("\nParse Tree:")
          // println(tree.toStringTree())
          
          println("\nParse Tree (pretty):")
          println(PTTools.prettyprint(tree))

          val code = io.Source.fromFile(fileName).mkString
          val ast = new ASTBuilder(tree, fileName, code).ast
          // println("\nAST:")
          // println(ast)

          val ast_pretty = new ASTPrinter(ast).print
          println("\nAST (pretty):")
          println(ast_pretty)
        }
    }

    // return value
    (error, tree) match {
      case (false, Some(tree)) => Some(tree)
      case _ => None
    }
  }

  def inter(fileName: String): Option[IR.ProgramIR] = {
      val parseTree = parse(fileName)
      val code = io.Source.fromFile(fileName).mkString
      parseTree match {
        case None => return None
        case Some(parseTree) =>

          // building an AST can't fail.
          val ast = new ASTBuilder(parseTree, fileName, code).ast

          val ir1 = new IRBuilder(ast).ir
          ir1 match {
            case Left(errors) =>
              Console.err.println(
                "\nFound %s semantic errors!".format(errors.length))
              errors.map(Console.err.println)
              return None
            case Right(ir1) =>
              // Semantic checks passed!
              if (CLI.debug) {
                println("\nIR (pretty):")
                println(IRPrinter.print(ir1))
              }
              return Some(ir1)
          }
      }
  }

  // Returns whether it worked.
  def assembly(fileName: String): Boolean = {
    // TODO appropriate output with/without debug
    val ir1 = inter(fileName)
    // Use map to go through stages.
    // Returns true or false based on whether all stages work.
    ir1.map{ ir1 => {
      val elaborated = Elaborate.elaborate(ir1)
      if (CLI.debug) {
        Console.err.println("\nElaborated IR (pretty):")
        Console.err.println(IRPrinter.print(elaborated))
      }
      val flattened = Flatten.flatten(elaborated)
      if (CLI.debug) {
        Console.err.println("\nFlattened IR (pretty):")
        Console.err.println(IRPrinter.print(flattened))
      }
      flattened
    }
    }.map{ ir1 =>
      val ir2 = new IR2Builder(ir1).ir2
      if (CLI.debug) {
        Console.err.println("\nIR2 (CFG):")
        Console.err.println(new IR2Printer(ir2).print)
      }
      ir2
    }.map{ ir2 =>
      val asm = new AsmGen(ir2).asm
      outFile.print(asm)
    }.isDefined
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
