package ast

import interp.Value
import scala.language.implicitConversions

enum Term extends AST:
  case Lit(value: Value)
  case Ifz(condition: Term, branch1: Term, branch2: Term)
  case BOp(token: Op, op1: Term, op2: Term)
  case Var(value: String)
  case Let(value: Var, t1: Term, t2: Term)
  case Fix(value: String, t1: Term)
  case Fun(value: String, t1: Term)
  case Call(t1: Term, t2: Term)

enum Op:
  case PLUS, MINUS, TIMES, DIVIDE

object Op:
  def parseOp(op: String): Op = op match {
    case "+" => PLUS
    case "-" => MINUS
    case "*" => TIMES
    case "/" => DIVIDE
  }