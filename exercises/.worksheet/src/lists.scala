import math.Ordering

object lists {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(80); 
  println("Welcome to the Scala worksheet");$skip(135); 

	def x(a: List[Int]): Int = a match {
	  case List() => 0
	  case List(1) => 1
	  case 1 :: b => b.size
	  case x :: 2 :: Nil => x
	};System.out.println("""x: (a: List[Int])Int""");$skip(23); val res$0 = 

  x(List(1, 2, 3, 4));System.out.println("""res0: Int = """ + $show(res$0));$skip(19); val res$1 = 
  x(List(1000, 2));System.out.println("""res1: Int = """ + $show(res$1));$skip(12); val res$2 = 
  x(List());System.out.println("""res2: Int = """ + $show(res$2));$skip(149); 

  // complexity is O(n*n)
  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  };System.out.println("""isort: (xs: List[Int])List[Int]""");$skip(156); 

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  };System.out.println("""insert: (x: Int, xs: List[Int])List[Int]""");$skip(176); 

  def removeAt[T](n: Int, xs: List[T]): List[T] =
    //if (xs.isEmpty) xs else if (n == 0) xs.tail else xs.head :: removeAt(n - 1, xs.tail)
    xs.take(n) ::: xs.drop(n + 1);System.out.println("""removeAt: [T](n: Int, xs: List[T])List[T]""");$skip(42); val res$3 = 
 
  removeAt(1, List('a', 'b', 'c', 'd'));System.out.println("""res3: List[Char] = """ + $show(res$3));$skip(256); 


  def merge[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]): List[T] =
    (xs, ys) match {
      case (Nil, y) => y
      case (x, Nil) => x
      case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    };System.out.println("""merge: [T](xs: List[T], ys: List[T])(implicit ord: scala.math.Ordering[T])List[T]""");$skip(199); 
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => {
      val (matches, rest) = xs1 span (y => x == y)
      (x :: matches) :: pack(rest)
    }
  };System.out.println("""pack: [T](xs: List[T])List[List[T]]""");$skip(53); 
  val data = List("a", "a", "a", "b", "c", "c", "a");System.out.println("""data  : List[String] = """ + $show(data ));$skip(13); val res$4 = 
  pack(data);System.out.println("""res4: List[List[String]] = """ + $show(res$4));$skip(91); 

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (x => (x.head, x.length));System.out.println("""encode: [T](xs: List[T])List[(T, Int)]""");$skip(15); val res$5 = 
  encode(data);System.out.println("""res5: List[(String, Int)] = """ + $show(res$5));$skip(105); 

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((t, u) => f(t) :: u);System.out.println("""mapFun: [T, U](xs: List[T], f: T => U)List[U]""");$skip(78); 

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((t, u) => 1 + u);System.out.println("""lengthFun: [T](xs: List[T])Int""");$skip(47); val res$6 = 

  mapFun[String, String](data, _.toUpperCase);System.out.println("""res6: List[String] = """ + $show(res$6));$skip(18); val res$7 = 
  lengthFun(data);System.out.println("""res7: Int = """ + $show(res$7))}
}
