import math.Ordering

object lists {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	def x(a: List[Int]): Int = a match {
	  case List() => 0
	  case List(1) => 1
	  case 1 :: b => b.size
	  case x :: 2 :: Nil => x
	}                                         //> x: (a: List[Int])Int

  x(List(1, 2, 3, 4))                             //> res0: Int = 3
  x(List(1000, 2))                                //> res1: Int = 1000
  x(List())                                       //> res2: Int = 0

  // complexity is O(n*n)
  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }                                               //> isort: (xs: List[Int])List[Int]

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }                                               //> insert: (x: Int, xs: List[Int])List[Int]

  def removeAt[T](n: Int, xs: List[T]): List[T] =
    //if (xs.isEmpty) xs else if (n == 0) xs.tail else xs.head :: removeAt(n - 1, xs.tail)
    xs.take(n) ::: xs.drop(n + 1)                 //> removeAt: [T](n: Int, xs: List[T])List[T]
 
  removeAt(1, List('a', 'b', 'c', 'd'))           //> res3: List[Char] = List(a, c, d)


  def merge[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]): List[T] =
    (xs, ys) match {
      case (Nil, y) => y
      case (x, Nil) => x
      case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }                                             //> merge: [T](xs: List[T], ys: List[T])(implicit ord: scala.math.Ordering[T])L
                                                  //| ist[T]
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => {
      val (matches, rest) = xs1 span (y => x == y)
      (x :: matches) :: pack(rest)
    }
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[String] = List(a, a, a, b, c, c, a)
  pack(data)                                      //> res4: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (x => (x.head, x.length))        //> encode: [T](xs: List[T])List[(T, Int)]
  encode(data)                                    //> res5: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())((t, u) => f(t) :: u) //> mapFun: [T, U](xs: List[T], f: T => U)List[U]

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((t, u) => 1 + u)             //> lengthFun: [T](xs: List[T])Int

  mapFun[String, String](data, _.toUpperCase)     //> res6: List[String] = List(A, A, A, B, C, C, A)
  lengthFun(data)                                 //> res7: Int = 7
}