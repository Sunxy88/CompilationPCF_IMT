package parser

import ast.*
import ast.Op
import lexer.{Op, Token, *}
import interp.Value

object Parser {
  def parseTerm(token: Token): Term = token match {
    case lexer.Lit(value) => Term.Lit(Value.IntValue(value))
    case LPar => parseComposite(Lexer.getToken)
    case Id(id) => Term.Var(id)
    case _ => throw new SyntaxError("Token: " + token.toString)
  }

  def getNTerms(n: Int): Array[Term] = {
    val res: Array[Term] = new Array[Term](n)
    for (i <- 0 until n) {
      res(i) = parseTerm(Lexer.getToken)
    }
    res
  }

  def parseComposite(token: Token): Term = token match {
    case KeyWord.IFZ =>
      val terms = getNTerms(3)
      checkRPar(Lexer.getToken)
      Term.Ifz(terms(0), terms(1), terms(2))
    case KeyWord.LET =>
      val terms = getNTerms(3)
      checkRPar(Lexer.getToken)
      Term.Let(terms(0).asInstanceOf[Term.Var], terms(1), terms(2))
    case KeyWord.FIX =>
      val id = Lexer.getToken
      val t = parseTerm(Lexer.getToken)
      checkRPar(Lexer.getToken)
      id match {
        case Id(id) => Term.Fix(id, t)
        case _ => throw new SyntaxError("Wrong token: " + id)
      }
    case KeyWord.FUN =>
      val id = Lexer.getToken
      val t = parseTerm(Lexer.getToken)
      checkRPar(Lexer.getToken)
      id match {
        case Id(id) => Term.Fun(id, t)
        case _ => throw new SyntaxError("Wrong token: " + id)
      }

    case lexer.Op.Plus => parseBOp(lexer.Op.Plus)
    case lexer.Op.Minus => parseBOp(lexer.Op.Minus)
    case lexer.Op.Times => parseBOp(lexer.Op.Times)
    case lexer.Op.Divide => parseBOp(lexer.Op.Divide)
    
    case _ =>
      val t1 = parseTerm(token)
      val t2 = parseTerm(Lexer.getToken)
      checkRPar(Lexer.getToken)
      Term.Call(t1, t2)
  }

  def parseBOp(op: lexer.Op): Term = {
    val op1 = parseTerm(Lexer.getToken)
    val op2 = parseTerm(Lexer.getToken)
    checkRPar(Lexer.getToken)
    op match {
      case lexer.Op.Plus => Term.BOp(ast.Op.PLUS, op1, op2)
      case lexer.Op.Minus => Term.BOp(ast.Op.MINUS, op1, op2)
      case lexer.Op.Times => Term.BOp(ast.Op.TIMES, op1, op2)
      case lexer.Op.Divide => Term.BOp(ast.Op.DIVIDE, op1, op2)
    }
  }

  def checkRPar(token: Token): Boolean = token match {
    case RPar => true
    case _ => throw new SyntaxError("Missing right parenthesis")
  }
}
