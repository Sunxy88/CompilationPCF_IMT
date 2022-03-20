package lexer

import java.io.{IOException, InputStream}

object Lexer:
	private var lexer: Lexer = null
	def apply(in: InputStream): Unit = lexer = new Lexer(in)
	def getToken: Token = lexer.getToken

class Lexer(in: InputStream) {
	private var i = in.read()// current ASCII character (coded as an integer)
	private var line = 1
	private var column = 1
	private val KEYWORD_MAP = Map(
		"LET" -> KeyWord.LET,
		"IFZ" -> KeyWord.IFZ,
		"FUN" -> KeyWord.FUN,
		"APP" -> KeyWord.APP,
		"FIX" -> KeyWord.FIX
	)
	private val OPERATOR_MAP = Map(
		'+' -> Op.Plus,
		'-' -> Op.Minus,
		'*' -> Op.Times,
		'/' -> Op.Divide
	)

	def lex(): List[Token] = {
		// return the list of tokens recorded in the file
		var tokens: List[Token] = Nil

		try {
			var token: Token = null // safe use of null will never be used
			token = getToken
			tokens ::= token
			while token != EOF do {
				token = getToken
				tokens ::= token
			}
		} catch {
			case e: IOException =>
				in.close() // close the reader
				throw e // pass the exception up the stack
		}
		tokens.reverse
	}

	def isBlank(i: Integer): Boolean = i == 10 || i == 32
	def isAllDigits(x: String): Boolean = x forall Character.isDigit
	def isOperator(x: Character): Boolean = OPERATOR_MAP.keySet.contains(x)
	def isKeyWord(x: String): Boolean = KEYWORD_MAP.contains(x)

	def getToken: Token = i match {
		case -1 => in.close(); EOF
		case ' ' | '\t' | '\n' => next(); getToken
		case '(' => next(); LPar
		case ')' => next(); RPar
		case _ if isOperator(i.toChar) => val tmp = i; next(); OPERATOR_MAP(tmp.toChar)
		case _ if i.toChar.isLetter => getIdentifierOrKeyword
		case _ if i.toChar.isDigit => getNumber
		case _ => throw new UnexpectedCharacter(i, line, column)
	}

	def next(): Unit = {
		i = in.read()
		column += 1
		if i == '\n' then {
			line += 1
			column = 0
		}
	}

	def getIdentifierOrKeyword: Token = {
		var res = "" + i.toChar
		next()
		while (i.toChar.isLetterOrDigit) {
			res += i.toChar
			next()
		}
		val upperCaseRes = res.toUpperCase()
		if isKeyWord(upperCaseRes) then KEYWORD_MAP(upperCaseRes) else Id(res)
	}

	def getNumber: Token = {
		var res = "" + i.toChar
		next()
		while (i.toChar.isDigit) {
			res += i.toChar
			next()
		}
		Lit(res.toInt)
	}

}
