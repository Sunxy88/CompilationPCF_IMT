package pcf

import java.io.FileInputStream
import java.io.InputStream

import lexer.{Lexer, Token}

object TestLexer {

	// args - arg[0] is the filename of the file to analyze
	// (if it exists, otherwise the standard input stream is used).

	def main(args: Array[String]): Unit = {
		var is: InputStream = System.in;
		if (args.length > 0) is = new FileInputStream(args(0))

		try {
			val lexer = new Lexer(is);
			val tokens: List[Token] = lexer.lex()
			// output of the result
			println(tokens)
			println(tokens.size + " token(s) found")
		} catch {
			case e: Exception => e.printStackTrace()
		}
	}
}
