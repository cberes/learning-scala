package week3

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
  def size: Int
  /** U is a supertype of T: return a list of the common supertype */
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  def size = 1 + tail.size
  def nth(n: Int) =
    if (n < 0) throw new IndexOutOfBoundsException("Cons.nth")
    else if (n == 0) head
    else tail.nth(n - 1)
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def size = 0
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int) = throw new IndexOutOfBoundsException("Nil.nth")
}

object Main {
  def singleton[T](elem: T) = new Cons[T](elem, Nil)

  def main(args: Array[String]) = {
    val list = new Cons(1, new Cons(2, new Cons(3, Nil)))
    println(list.size)
    println(list.nth(2))
    //println(list.nth(4))
    println(list.nth(-1))
  }
}
