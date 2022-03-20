package interp

import ast.{Op, Term}
import interp.{IceCube, Value, int2Value, value2Int}
import parser.SyntaxError
import typer.*

import java.io.{FileWriter, InputStream}

object Interp {
  type Env = Map[String, Value | IceCube]

  private def uniftyAndTest(type1: Type, type2: Type): Unit =
    if !(type1 === type2) then throw new TypeError(s"Not matched types: $type1 and $type2")

  def typer(term: Term, prevEnv: Map[String, Type]): Type = {
    term match {
      case Term.Lit(value) => value match {
        case Value.IntValue(value) => INT.deref
        case Value.Closure(value, term, venv) => typer(term, prevEnv)
      }
      case Term.Ifz(condition, branch1, branch2) =>
        val typeCondition: Type = typer(condition, prevEnv)
        val typeBranch1: Type = typer(branch1, prevEnv)
        val typeBranch2: Type = typer(branch2, prevEnv)
        uniftyAndTest(typeCondition, INT)
        uniftyAndTest(typeBranch1, typeBranch2)
        typeBranch1
      case Term.BOp(token, op1, op2) =>
        val type1 = typer(op1, prevEnv)
        val type2 = typer(op2, prevEnv)
        uniftyAndTest(type1, type2)
        type1
      case Term.Var(value) =>
        val typeValue = variableType(value, prevEnv)
        typeValue
      case Term.Let(value, t1, t2) =>
        val typeT1 = typer(t1, prevEnv)
        value match {
          case Term.Var(name) => return typer(t2, prevEnv + (name -> typeT1))
        }
        typer(t2, prevEnv)
      case Term.Fix(value, t1) =>
        val typeValue = variableType(value, prevEnv)
        val termValue = typer(t1, prevEnv)
        uniftyAndTest(typeValue, termValue)
        termValue
      case Term.Fun(value, t1) =>
        val typeValue = TVar()
        val termValue: Type = typer(t1, prevEnv + (value->typeValue))
        typeValue --> termValue
      case Term.Call(t1, t2) =>
        val t1Type = typer(t1, prevEnv)
        val t2Type = typer(t2, prevEnv)
        val xType = TVar()
        val methodType = t2Type --> xType
        if t1Type === methodType then xType else throw new TypeError(s"Not matched types: $t1Type and $methodType")
    }
  }

  def variableType(variable: String, env: Map[String, Type]): Type =
    if env.contains(variable) then env(variable) else TVar()

  def interp(t: Term, prevEnv: Env = Map()): Value = {
    val curEnv = collection.mutable.Map() ++ prevEnv // Get all previous variables
    t match {
      case Term.Lit(value) => value
      case Term.Ifz(condition, branch1, branch2) =>
        if value2Int(interp(condition, prevEnv)) == 0 then interp(branch1, prevEnv) else interp(branch2, prevEnv)
      case Term.BOp(token, op1, op2) => token match {
        case Op.PLUS => interp(op1, prevEnv) + interp(op2, prevEnv)
        case Op.MINUS => interp(op1, prevEnv) - interp(op2, prevEnv)
        case Op.TIMES => interp(op1, prevEnv) * interp(op2, prevEnv)
        case Op.DIVIDE =>
          val valOp2 = interp(op2, prevEnv)
          if value2Int(valOp2) != 0
          then interp(op1, prevEnv) / valOp2
          else throw new InterpretationError("divide by zero")
      }
      case Term.Var(value) =>
        if curEnv.keySet.contains(value)
        then
          curEnv(value) match {
            case Value.IntValue(v) => v
            case Value.Closure(v, term, env) => Value.Closure(v, term, env)
            case IceCube.IceCube(v, term) =>
              interp(term, curEnv.toMap)
          }
        else throw new InterpretationError("variable doesn't exist: " + value)
      case Term.Let(value, t1, t2) =>
        val valName = varName(value)
        curEnv(valName) = interp(t1, prevEnv) // Update the variable
        interp(t2, curEnv.toMap)
      case Term.Fun(value, t1) =>
        val clos = Value.Closure(value, t1, curEnv.toMap)
        curEnv(value) = clos
        clos
      case Term.Call(t1, t2) =>
        val v2 = interp(t2, curEnv.toMap)
        val v1 = interp(t1, curEnv.toMap)
        v1 match {
          case Value.IntValue(value) => value
          case Value.Closure(value, term, env) =>
            val closEnv = collection.mutable.Map() ++ env ++ Map(value->v2)
            interp(term, closEnv.toMap)
        }
      case Term.Fix(funName, t1) =>
        curEnv(funName) = IceCube.IceCube(funName, t1)
        interp(t1, curEnv.toMap)
    }
  }

  def varName(v: Term.Var): String = v match {
    case Term.Var(value) => value
  }
}
