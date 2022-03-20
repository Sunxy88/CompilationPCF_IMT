package pcf

import interp.Interp
import lexer.{Lexer, Token}
import parser.Parser
import typer.Type
import annotate.ATerm
import annotate.ATerm.gen

import java.io.{FileInputStream, FileWriter, InputStream}

object Interp {
  val SHELL = "bash"

  def main(args: Array[String]): Unit = {
    var is: InputStream = System.in
    if (args.length > 0) is = new FileInputStream(args(0))

    compile(is, Option.apply("test/test.wat"))
    test("wat2wasm test/test.wat -o test/test.wasm")
//
//    try {
//      Lexer(is)
//      val tok = Lexer.getToken
//      val root = Parser.parseTerm(tok)
//      val aterm = ATerm.annotate(root)
//      println(aterm)
//      val a = interp.Interp.typer(root, Map[String, Type]())
//      val result = interp.Interp.interp(root)
//      println(root)
//      println(s"===> ${(result, a)}")
//    } catch {
//      case e: Exception => e.printStackTrace()
//    }
  }

  def analyze(is: InputStream): ATerm = {
    try {
      Lexer(is)
      val tok = Lexer.getToken
      val root = Parser.parseTerm(tok)
      val aterm = ATerm.annotate(root)
      println(aterm)
      aterm
    } catch {
      case e: Exception => throw e
    }
  }

  def compile(is: InputStream, inputFile: Option[String]) : Unit = {
    val term = analyze(is: InputStream)
    val code: String =  "(module (func (export \"main\") (result i32)\n"
      + s"   ${gen(term)}\n"
      + "   return)\n"
      + "   )\n"
    if (inputFile.isDefined) write(code, inputFile.get)
    else println(code)
  }

  // write code to .wat file associated to .pcf file passed as argument,
  // returning .wat file relative filename
  def write(code: String, filename: String) = {
    val CFilename = filename.replaceFirst("\\.pcf\\z", ".wat")
//    if (verbose) System.out.println("writing .wat code to " + CFilename)
    val out = new FileWriter(CFilename)
    out.write(code)
    out.flush()
    out.close()
    CFilename
  }

  def test(shellCmd: String): Unit = {
    val cmd = Array(SHELL, "-c", shellCmd)
    Runtime.getRuntime.exec(cmd).waitFor
  }
}
