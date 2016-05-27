import scala.language.higherKinds

//Bool
sealed trait BOOL {
  def &&(b: BOOL): BOOL //def(b: Bool): Bool
  def ||(b: BOOL): BOOL //def(b: Bool): Bool
  def IfElse[B](t: => B, f: => B): B // def[B](t: => B, f: => B): B
}

lazy val TRUE: BOOL = new BOOL {
  def &&(b: BOOL) = b
  def ||(b: BOOL) = TRUE
  def IfElse[B](t: => B, f: => B) = t
}
lazy val FALSE: BOOL = new BOOL {
  def &&(b: BOOL) = FALSE
  def ||(b: BOOL) = b
  def IfElse[B](t: => B, f: => B) = f
}

assert((FALSE && TRUE) == FALSE)
assert((FALSE IfElse (1,3)) == 3)
///////////////////////////////////////////////////
//
//def f(b:B): B
//type f[B] = B
//type f[B <: X] = B
//type f[B <: X] <: X
//
trait BOOL2 {
  type &&[B <: BOOL2] <: BOOL2
  type ||[B <: BOOL2] <: BOOL2
  type IfElse[B, T <: B, F <: B] <: B
}

object TRUE2 extends BOOL2{
  type &&[B <: BOOL2] = B
  type ||[B <: BOOL2] = TRUE2.type
  type IfElse[B, T <: B, F <: B] = B
}
object FALSE2 extends BOOL2{
  type &&[B <: BOOL2] = FALSE2.type
  type ||[B <: BOOL2] = B
  type IfElse[B, T <: B, F <: B] = F //only one parameter list on type lvl
}

// =:= <- not operator, class
implicitly[(FALSE2.&&[FALSE2.type]) =:= FALSE2.type] // false && false == true
//implicitly[(FALSE2.&&[FALSE2.type]) =:= TRUE.type] // false && false == true
implicitly[(FALSE2.IfElse[Any, Int, String]) =:= String] // false && false == true
