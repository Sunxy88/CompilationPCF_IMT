package lexer

class UnexpectedCharacter(i: Int, line: Int, column: Int) extends Exception(s"unexpected character : ascii $i -" +
  s" char ${i.toChar} around line: $line, column: $column")
