package pcf

import lexer.{Lexer, Token}
import parser.Parser

import java.io.{FileInputStream, InputStream}

object TestParser {
  def main(args: Array[String]): Unit = {
    var is: InputStream = System.in
    if (args.length > 0) is = new FileInputStream(args(0))

    try {
      Lexer(is)
      val tok = Lexer.getToken
      println(Parser.parseTerm(tok))
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }
}
