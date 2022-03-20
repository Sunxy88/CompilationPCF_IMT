package lexer

sealed trait Token:
  var line = 0
  var column = 0
case object EOF extends Token
case object LPar extends Token // '('
case object RPar extends Token // ')'
//case object Ifz extends Token
case class Lit(value: Integer) extends Token // literal
case class Id(id: String) extends Token // identifier

enum Op extends Token:
  case Plus, Minus, Times, Divide

enum KeyWord extends Token:
  case FUN, APP, IFZ, FIX, LET
