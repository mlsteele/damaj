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

  // Maps optimization namse to the actual optimization function
  // This is used by CLI.parse
  type Optimization = (String, IR2.Program => IR2.Program)
  val optimizations:List[Optimization] = List(
    ("deadcode", DeadCodeElim(_))
  )

  var outFile = Console.out

  def enabledOptimizations() : List[Optimization]= {
    var opts:List[Optimization] = List()
    for (i <- CLI.opts.indices) {
      if (CLI.opts(i)) {
        opts = opts :+ optimizations(i)
      }
    }
    return opts
  }

  def section(string: String) = {
    Console.err.println("="*40)
    Console.err.println(string)
    Console.err.println("="*40)
  }

  def main(args: Array[String]): Unit = {
    CLI.parse(args, optimizations.map(_._1).toArray);
    outFile = Option(CLI.outfile) match {
      case None => Console.out
      case Some(file) => new java.io.PrintStream(new java.io.FileOutputStream(file))
    }
    if (CLI.infile == null || !(new java.io.File(CLI.infile).exists)){
        Console.err.println(CLI.infile + ": No sruh file or directory")
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
          // Console.err.println("\nParse Tree:")
          // Console.err.println(tree.toStringTree())

          section("Parse Tree (pretty):")
          Console.err.println(PTTools.prettyprint(tree))

          val code = io.Source.fromFile(fileName).mkString
          val ast = new ASTBuilder(tree, fileName, code).ast

          // Console.err.println("AST:")
          // Console.err.println(ast)

          val ast_pretty = new ASTPrinter(ast).print
          section("AST (pretty):")
          Console.err.println(ast_pretty)
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
                section("IR (pretty):")
                Console.err.println(IRPrinter.print(ir1))
              }
              return Some(ir1)
          }
      }
  }

  def applySimplification(pass : (IR.ProgramIR => IR.ProgramIR), name: String) (ir: IR.ProgramIR) : IR.ProgramIR = {
    // Apply the simplification pass
    val simp = pass(ir);
    // Print the resulting pass
    if (CLI.debug) {
      section("%s Pass:".format(name))
      Console.err.println(IRPrinter.print(simp))
    }
    return simp
  }

  type SimplificationPass = (IR.ProgramIR => IR.ProgramIR)

  // Returns whether it worked.
  def assembly(fileName: String): Boolean = {
    // TODO appropriate output with/without debug
    val ir1 = inter(fileName)
    // Use map to go through stages.
    // Returns true or false based on whether all stages work.
    val simplification_passes = List(
      (Elaborate.elaborate(_:IR.ProgramIR)         , "Elaboration"),
      (Desugar.desugar(_:IR.ProgramIR)             , "Desugaring"),
      (Flatten.flatten(_:IR.ProgramIR)             , "Expression Flattening"),
      (CombineScopes.combineScopes(_:IR.ProgramIR) , "Scope Combination")
    ).map(x => Function.tupled(applySimplification _)(x))

    // Combine each individual pass into one large pass
    val simplify = simplification_passes.reduce(_ andThen _)

    ir1.map(simplify).map{ ir1 =>
      section("IR1 -> IR2")
      val ir2 = new IR2Builder(ir1).ir2
      if (CLI.debug) {
        Grapher.graph(ir2, "1-raw")
      }
      ir2
     }.map { ir2 =>
       section("Condensing")
       val condensed = Condense.transform(ir2)
       if (CLI.debug) {
         Grapher.graph(condensed, "2-condensed")
       }
       condensed
    }.map { ir2 =>
      if (CLI.debug) {
        section("Optimization")
        Console.err.println("Enabled optimizations:")
        enabledOptimizations.foreach {opt => Console.err.println(opt._1)}
      }
      var tempIR = ir2
      for (opt <- enabledOptimizations) opt match {
        case (optName, optFunc) => 
          if (CLI.debug) {
            section("Applying %s optimization".format(optName))
          }
          tempIR = optFunc(tempIR)
      }
      tempIR
    }.map { ir2 =>
      section("Generating Assembly")
      new AsmGen(ir2).asm
    }.map { asm =>
      section("Writing to output")
      outFile.print(asm)
    }.isDefined
  }
}
