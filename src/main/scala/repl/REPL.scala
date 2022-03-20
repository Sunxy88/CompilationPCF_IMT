package repl

import ast.{Op, Term}
import interp.{Value, IceCube, value2Int, int2Value}
import interp.Interp.{Env, interp, varName}
import _root_.interp.InterpretationError
import lexer.{Lexer, Token}
import scala.annotation.tailrec

/**
 * Ce REPL fonctionne comme cela:
 * > (let x 1 0)
 * Map(x -> 1)
 * > (let y 1 0)
 * Map(x -> 1, y -> 1)
 * > (+ x y)
 * ====> 2
 * Map(x -> 1, y -> 1)
 * (let fact (fix f (fun n (ifz n 1 (* n (f (- n 1)))))) 0)
 * (fact 3)
 */

object REPL extends App :
  print("> ")
  Lexer(System.in)
  REPL(lexer.Lexer.getToken, Map())

  @tailrec
  def REPL(tok: Token, e: Env): Unit = tok match
    case lexer.EOF => System.exit(0)
    case _ =>
      val e1 = _REPL(tok, e)
      println(e1)
      print("> ")
      REPL(lexer.Lexer.getToken, e1)

  private def _REPL(tok: Token, e: Env): Env = {
    val term = parser.Parser.parseTerm(tok)
    val curEnv = collection.mutable.Map() ++ e
    term match {
      case Term.Lit(value) => formatPrint(value)
      case Term.BOp(opd, op1, op2) =>
        var value: Int = 0
        opd match {
          case Op.PLUS => value = interp(op1, e) + interp(op2, e)
          case Op.MINUS => value = interp(op1, e) - interp(op2, e)
          case Op.TIMES => value = interp(op1, e) * interp(op2, e)
          case Op.DIVIDE =>
            val op2Val = interp(op2, e)
            if value2Int(op2Val) == 0
            then
              println("Divide by zero")
              return curEnv.toMap
            else
            value = interp(op1, e) / op2Val
        }
        formatPrint(value)
      case Term.Var(value) =>
        if curEnv.keySet.contains(value)
        then
          formatPrint(curEnv(value))
        else
          println("Variable doesn't exist")
      case Term.Let(value, t1, t2) =>
        val valName = varName(value)
        curEnv(valName) = interp(t1, curEnv.toMap)
      case Term.Fun(value, t1) =>
        val clos = Value.Closure(value, t1, curEnv.toMap)
        curEnv(value) = clos
      case Term.Call(t1, t2) =>
        val v2 = interp(t2, curEnv.toMap)
        val v1 = interp(t1, curEnv.toMap)
        v1 match {
          case Value.IntValue(value) => formatPrint(value)
          case Value.Closure(value, term, env) =>
            val closEnv = collection.mutable.Map() ++ env ++ Map(value->v2)
            formatPrint(interp(term, closEnv.toMap))
        }
      case Term.Fix(funName, t1) =>
        curEnv(funName) = IceCube.IceCube(funName, t1)
        formatPrint(interp(t1, curEnv.toMap))
      case _ => throw new InterpretationError("Not supported yet")
    }
    curEnv.toMap
  }

  private def formatPrint(value: Value | IceCube | Int): Unit = println(s"====> $value")
