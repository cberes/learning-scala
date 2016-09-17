object intsets {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala worksheet");$skip(27); 
  val t1 = new NonEmpty(5);System.out.println("""t1  : NonEmpty = """ + $show(t1 ));$skip(46); 
  val t2 = t1 add 13 add 12 add 8 add 7 add 9;System.out.println("""t2  : IntSet = """ + $show(t2 ));$skip(27); 
  val s1 = new NonEmpty(7);System.out.println("""s1  : NonEmpty = """ + $show(s1 ));$skip(46); 
  val s2 = s1 add 5 add 12 add 9 add 13 add 8;System.out.println("""s2  : IntSet = """ + $show(s2 ));$skip(27); 
  val u1 = new NonEmpty(4);System.out.println("""u1  : NonEmpty = """ + $show(u1 ));$skip(20); 
  val u2 = u1 add 3;System.out.println("""u2  : IntSet = """ + $show(u2 ));$skip(14); val res$0 = 
  s2 union u2;System.out.println("""res0: IntSet = """ + $show(res$0))}
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
