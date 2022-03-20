package interp

import ast.{AST, Term}

enum Value:
  case IntValue(value: Int)
  case Closure(value: String, term: Term, env: Map[String, Value|IceCube])

enum IceCube:
  case IceCube(value: String, term: Term)


given value2Int: Conversion[Value, Int] with {
  override def apply(x: Value): Int = x match {
    case Value.IntValue(value) => value
    case Value.Closure(value, term, env) => throw new Error
  }
}

given int2Value: Conversion[Int, Value] with {
  override def apply(x: Int): Value = Value.IntValue(x)
}

given closure2String: Conversion[Value.Closure, String] with {
  override def apply(clos: Value.Closure): String = s"Closure(${clos.value},${clos.term},${clos.env})"
}

