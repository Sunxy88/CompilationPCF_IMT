package typer

import scala.annotation.tailrec

/**
 * @author Jacques Noye
 * @version 0.4
 * @since 2022-03-15
 */
// fragile, especially with respect to checking cycles (occur check)
// TODO: improve encapsulation so that deref is private to the Type hierarchy
// TODO: hide the implementation of TVar?

sealed trait Type :
  def ===(that: Type): Boolean // unifies "this" and "that", returns true in case of success, false otherwise
  def -->(r: Type) = new -->(this, r) // creates an arrow type
  private[typer] def deref: Type // returns the dereferenced value of "this"

private object Type :
  def occurCheck(x: TVar, a: Type): Boolean =
    val r = _occurCheck(x, a)
    if !r then println(s"$x occurs in $a")
    r
  def _occurCheck(x: TVar, a: Type): Boolean = a.deref match
    case INT => true
    case b --> c => occurCheck(x, b) && occurCheck(x, c)
    case y@TVar(_) =>
      // a has been dereferenced, y is unbound
      if y eq x then false
      else y.binding match
        case Some(b) => occurCheck(x, b)
        case None => true

case object INT extends Type :
  def deref: Type = INT
  def ===(b: Type): Boolean = b.deref match
    case INT => true
    case b1:TVar => b1.binding = Some(INT); true
    case _ => false

object TVar :
  private var seed = 0
  private def gen: String = { val s = "X" + seed; seed += 1; s }
  def apply() = new TVar(gen)

private case class TVar(name: String) extends Type :
  var binding: Option[Type] = None // initially an unbound variable
  def deref: Type = binding match
    case None => this // unbound variable
    case Some(a) => a.deref // retrieve derefenced binding
  def ===(that: Type): Boolean = (this.deref, that.deref) match
    case (INT, INT) => true
    case (INT, b:TVar) => b.binding = Some(INT); true
    case (a1 --> b1, a2 --> b2) => a1 === a2 && b1 === b2
    case (a: -->, b:TVar) =>
      if Type.occurCheck(b, a) then { b.binding = Some(a); true }
      else false
    // the following case (two identical unbound variables) can be avoided by implementing unbound
    // variables as self-references
    // otherwise if this case is forgotten, cycles are created, which result in infinite dereferencing
    case (a:TVar, b:TVar) if a eq b => true
    case (a:TVar, b) =>
      if Type.occurCheck(a, b) then { a.binding = Some(b); true }
      else false
    case _ => false
  override def toString: String = binding match
    case None => s"$name"
    case Some(t) => t.deref.toString

case class -->(a: Type, b: Type) extends Type :
  def deref: Type = this
  def ===(c: Type): Boolean = c.deref match
    case c1:TVar => c1 === this
    case a1 --> b1 => a === a1 && b === b1
    case _ => false
  override def toString: String = s"($a -> $b)"

object Test extends App :
  val a = INT
  val b = INT
  println(s"Is a === b true? ${a === b}")
  val v = TVar()
  println(s"variable v created, v = $v")
  println(s"Is v === a true? ${v === a}")
  println(s"v = $v")
  val v1 = TVar()
  println(s"variable v1 created, v1 = $v1")
  val v2 = TVar()
  println(s"variable v2 created, v2 = $v2")
  println(s"Is v1 === v2 true? ${v1 === v2}")
  println(s"v1 = $v1")
  println(s"v2 = $v2")
  println(s"Is v1 === v2 true? ${v1 === v2}")
  println(s"Is v1 === a true? ${v1 === a}")
  println(s"v1 = $v1")
  println(s"v2 = $v2")
  val v3 = TVar()
  val v4 = TVar()
  val a1 = v3 --> TVar()
  val a2 = TVar() --> v4
  println(s"a1 = $a1")
  println(s"a2 = $a2")
  v3 === v4
  println("v3 === v4")
  println(s"a1 = $a1")
  println(s"a2 = $a2")
  a1 === a2
  println("a1 === a2")
  println(s"a1 = $a1")
  println(s"a2 = $a2")
  v3 === INT
  println(s"a1 = $a1")
  println(s"a2 = $a2")

