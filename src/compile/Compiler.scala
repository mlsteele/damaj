package compile
import util.CLI
import scala.util.parsing.input.Reader
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.PagedSeq
import java.io._
import scala.io.Source
import scala.collection.mutable.{StringBuilder, ListBuffer}
import scala.Console

// Begin parser/scanner imports
import antlr.CommonAST
import antlr.collections.AST
import antlr.Token
import edu.mit.compilers.grammar.{ DecafParser, DecafParserTokenTypes, DecafScanner, DecafScannerTokenTypes }

object Compiler {
  val tokenMap = Map(
    DecafScannerTokenTypes.IDENTIFIER -> "IDENTIFIER",
    DecafScannerTokenTypes.CHARLITERAL -> "CHARLITERAL",
    DecafScannerTokenTypes.STRINGLITERAL -> "STRINGLITERAL",
    DecafScannerTokenTypes.INTLITERAL -> "INTLITERAL",
    DecafScannerTokenTypes.TK_true -> "BOOLEANLITERAL",
    DecafScannerTokenTypes.TK_false -> "BOOLEANLITERAL"
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
      val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      scanner.setTrace(CLI.debug)
      var done = false
      while (!done) {
        try {
          val head = scanner.nextToken()
          if (head.getType() == DecafScannerTokenTypes.EOF) {
            done = true
          } else {
            val tokenType = tokenMap.getOrElse(head.getType(), "")
            // outFile.println(head.getLine() + (if (tokenType ==  "") "" else " ") + tokenType + " " + head.getText())
            outFile.println(
              head.getLine()
              + (if (tokenType ==  "") "" else " ")
              // + " (" + head.getType() + ") "
              + tokenType
              + " "
              + head.getText())
          }
        } catch {
          case ex: Exception => {
            Console.err.println(CLI.infile + " " + ex)
            scanner.consume();
          }
        }
      }
    } catch {
      case ex: Exception => Console.err.println(ex)
    }
  }

  def parse(fileName: String): Option[CommonAST]  = {
    /** 
    Parse the file specified by the filename. Eventually, this method
    may return a type specific to your compiler.
    */
    var inputStream : java.io.FileInputStream = null
    try {
      inputStream = new java.io.FileInputStream(fileName)
    } catch {
      case f: FileNotFoundException => { Console.err.println("File " + fileName + " does not exist"); return null }
    }

    val scanner = new DecafScanner(new DataInputStream(inputStream))
    val parser = new DecafParser(scanner);
    parser.setTrace(CLI.debug)
    parser.program()
    val tree = Option(parser.getAST().asInstanceOf[CommonAST])
    val error: Boolean = parser.getError()

    (error, tree) match {
      case (true, _) =>
        Console.err.println("[ERROR]: Parse error\n")
      case (false, None) =>
        Console.err.println("[ERROR] No parse tree but no error\n")
      case (false, Some(tree)) =>
        Console.err.println("[YAY] Parse succeeded\n")
    }

    tree
  }
}
