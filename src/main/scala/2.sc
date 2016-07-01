import scala.language.higherKinds
import scala.language.postfixOps

sealed trait Number
final class Successor[A <: Number] extends Number

final class p0 extends Number
type p1 = Successor[p0]
type p2 = Successor[p1]
type p3 = Successor[p2]
type p4 = Successor[p3]
type p5 = Successor[p4]
type p6 = Successor[p5]
type p7 = Successor[p6]
type p8 = Successor[p7]
type p9 = Successor[p8]
type p10 = Successor[p9]

// Number comparators
trait ||[A, B]
implicit def leftOr[A, B](implicit ev: A) = new (A || B) {}
implicit def rightOr[A, B](implicit ev: B) = new (A || B) {}

trait <[A, B]
implicit def zeroLessThanSucc[A <: Number] = new (p0 < Successor[A]) {}
implicit def succLessThanSucc[A <: Number, B <: Number](implicit ev: A < B) = new (Successor[A] < Successor[B]) {}

trait >[A, B]
implicit def succGreaterThanZero[A <: Number] = new (Successor[A] > p0) {}
implicit def succGreaterThanSucc[A <: Number, B <: Number](implicit ev: A > B) = new (Successor[A] > Successor[B]) {}

type <=[A, B] = (A < B) || (A =:= B)
type >=[A, B] = (A > B) || (A =:= B)

implicitly[p2 > p1]
//implicitly[p2 < p1] //will not compile