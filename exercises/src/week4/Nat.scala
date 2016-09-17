package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new NonZero(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor: Nat = throw new UnsupportedOperationException("Zero.predecessor")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new UnsupportedOperationException("Zero.-")
  override def toString = ""
}

class NonZero(previous: Nat) extends Nat {
  def isZero = false
  def predecessor: Nat = previous
  def + (that: Nat): Nat = new NonZero(predecessor + that)
  def - (that: Nat): Nat =
    if (that.isZero) this else predecessor - that.predecessor
  override def toString = predecessor.toString + "+"
}

object NatMain extends App {
  println((Zero.successor + Zero.successor.successor).toString)
  println((Zero.successor.successor.successor - Zero.successor.successor).toString)
}