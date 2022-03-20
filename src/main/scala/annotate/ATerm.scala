package annotate

import ast.{AST, Op, Term}
import interp.Value
import parser.SyntaxError

import java.io.{FileWriter, InputStream}

type AEnv = Array[ATerm]

enum ATerm extends AST:
  case Lit(value: Value)
  case Ifz(condition: ATerm, branch1: ATerm, branch2: ATerm)
  case BOp(token: Op, op1: ATerm, op2: ATerm)
  case Var(index: Int, value: String)
  case Let(value: ATerm.Var, t1: ATerm, t2: ATerm)
  case Fix(value: ATerm.Var, t1: ATerm)
  case Fun(value: ATerm.Var, t1: ATerm)
  case Call(t1: ATerm, t2: ATerm)

//class ATerm(val index: Int, val t: Term):
//  def term: Term = t

object ATerm:
  def annotate(term: Term, env: AEnv = Array[ATerm]()): ATerm = term match {
    case Term.Lit(value) => value match {
      case Value.IntValue(v) => v
      case Value.Closure(v, term, env) => 0 // TODO
    }
    case Term.Ifz(condition, branch1, branch2) =>
      val cond = annotate(condition, env)
      env :+ cond
      val brc1 = annotate(branch1, env)
      env :+ brc1
      val brc2 = annotate(branch2, env)
      env :+ brc1
      ATerm.Ifz(cond, brc1, brc2)
    case Term.BOp(token, op1, op2) =>
      val operand1 = annotate(op1, env)
      env :+ operand1
      val operand2 = annotate(op2, env)
      env :+ operand2
      ATerm.BOp(token, operand1, operand2)
    case Term.Var(value) =>
      val v = ATerm.Var(env.length, value)
      env :+ v
      v
    case Term.Let(value, t1, t2) =>
      val t = annotate(t1, env)
      env :+ t
      value match {
        case Term.Var(name) =>
          val v = ATerm.Var(env.length, name)
          env :+ v
          ATerm.Let(v.asInstanceOf[ATerm.Var], t, annotate(t2, env))
        case _ => throw new SyntaxError(s"wrong type: $value")
      }
    case Term.Fix(value, t1) =>
      val para = ATerm.Var(env.length, value)
      env :+ para
      ATerm.Fix(para.asInstanceOf[ATerm.Var], annotate(t1, env))
    case Term.Fun(value, t1) =>
      val para = ATerm.Var(env.length, value)
      env :+ para
      ATerm.Fun(para.asInstanceOf[ATerm.Var], annotate(t1, env))
    case Term.Call(t1, t2) =>
      val caller = annotate(t1, env)
      env :+ caller
      val para = annotate(t2, env)
      env :+ para
      ATerm.Call(caller, para)
  }

  def gen(aterm:ATerm): String = aterm match {
    case ATerm.Lit(value) =>
      value match {
        case Value.IntValue(value) => s"(i32.const $value)\n"
        case Value.Closure(value, term, env) => ""
      }
    case ATerm.Ifz(condition, branch1, branch2) =>
      s"${ATerm.gen(condition)}\n" + "if (result i32)\n" + s"${ATerm.gen(branch2)}\n" + "else\n" + s"${ATerm.gen(branch1)}\n" + "end"
    case ATerm.BOp(token, op1, op2) => token match {
      case Op.PLUS => s"${ATerm.gen(op1)} ${ATerm.gen(op2)} (i32.add)\n"
      case Op.MINUS => s"${ATerm.gen(op1)} ${ATerm.gen(op2)} (i32.sub)\n"
      case Op.TIMES => s"${ATerm.gen(op1)} ${ATerm.gen(op2)} (i32.mul)\n"
      case Op.DIVIDE => s"${ATerm.gen(op1)} ${ATerm.gen(op2)} (i32.div_u)\n"
    }
    case ATerm.Var(index, value) =>
      "(local.get $" + s"$value$index)\n"
    case ATerm.Let(value, t1, t2) =>
      value match {
        case ATerm.Var(index, name) =>   "(local $" + s"$name i32)\n" + "(local.set $" + s"$name ${ATerm.gen(t1)})\n" + s"${ATerm.gen(t2)}"
      }

//    case ATerm.Fix(value, t1) =>
    case ATerm.Fun(value, t1) =>
      value match {
        case ATerm.Var(index, name) =>
          "(func (param $" + s"$name$index" + " i32)\n"+ s"${gen(t1)})\n"
      }
//    case ATerm.Call(t1, t2) =>

  }

given int2ATerm: Conversion[Int, ATerm] with {
  override def apply(x: Int): ATerm = ATerm.Lit(Value.IntValue(x))
}