object intsets {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val t1 = new NonEmpty(5)                        //> t1  : NonEmpty = {.5.}
  val t2 = t1 add 13 add 12 add 8 add 7 add 9     //> t2  : IntSet = {.5{{{{.7.}8{.9.}}12.}13.}}
  val s1 = new NonEmpty(7)                        //> s1  : NonEmpty = {.7.}
  val s2 = s1 add 5 add 12 add 9 add 13 add 8     //> s2  : IntSet = {{.5.}7{{{.8.}9.}12{.13.}}}
  val u1 = new NonEmpty(4)                        //> u1  : NonEmpty = {.4.}
  val u2 = u1 add 3                               //> u2  : IntSet = {{.3.}4.}
  s2 union u2                                     //> res0: IntSet = {{.3.}4{{.5{.7.}}8{.9{{.12.}13.}}}}
}

abstract class IntSet {
  def add(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(x: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false
  def add(x: Int): IntSet = new NonEmpty(x)
  def union(x: IntSet): IntSet = x
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def this(elem: Int) = this(elem, Empty, Empty)

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def add(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left add x, right)
    else if (x > elem) new NonEmpty(elem, left, right add x)
    else this

  def union(x: IntSet): IntSet =
    ((left union right) union x) add elem // wtf

  override def toString = "{" + left + elem + right + "}"
}