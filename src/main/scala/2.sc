import scala.language.higherKinds
import scala.language.postfixOps

sealed trait Natural {
  def ++ = new Successor(this)
  type * [_ <: Natural] <: Natural
  type + [_ <: Natural] <: Natural
}
object Zero extends Natural {
  type * [X <: Natural] = Zero.type
  type + [X <: Natural] = X
}

class Successor[N <: Natural] extends Natural {
  type + [X <: Natural] = Successor[N + X]
  type * [X <: Natural] = Successor[N * X + X]
}

val _0 = Zero
val _1 = _0 ++
val _2 = _1 ++
////////////////////////////////////////
sealed trait Number {
//  type This[T :> this.type <: Number]
//  type ++ = Successor[This]
}
final class Successor2[A <: Number] extends Number

final class p0 extends Number
type p1 = Successor2[p0]
type p2 = Successor2[p1]
type p3 = Successor2[p2]
type p4 = Successor2[p3]
type p5 = Successor2[p4]
type p6 = Successor2[p5]
type p7 = Successor2[p6]
type p8 = Successor2[p7]
type p9 = Successor2[p8]
type p10 = Successor2[p9]

// Number comparators
trait ||[A, B]
implicit def leftOr[A, B](implicit ev: A) = new (A || B) {}
implicit def rightOr[A, B](implicit ev: B) = new (A || B) {}

trait <[A, B]
implicit def zeroLessThanSucc[A <: Number] = new (p0 < Successor2[A]) {}
implicit def succLessThanSucc[A <: Number, B <: Number](implicit ev: A < B) = new (Successor2[A] < Successor2[B]) {}

trait >[A, B]
implicit def succGreaterThanZero[A <: Number] = new (Successor2[A] > p0) {}
implicit def succGreaterThanSucc[A <: Number, B <: Number](implicit ev: A > B) = new (Successor2[A] > Successor2[B]) {}

type <=[A, B] = (A < B) || (A =:= B)
type >=[A, B] = (A > B) || (A =:= B)

implicitly[p2 > p1]
//implicitly[p2 < p1]