package week7

object infinitestreams {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(91); 
  def from(n: Int): Stream[Int] = n #:: from(n + 1);System.out.println("""from: (n: Int)Stream[Int]""");$skip(22); 

  val nats = from(0);System.out.println("""nats  : Stream[Int] = """ + $show(nats ));$skip(30); 

  val m4s = nats map (_ * 4);System.out.println("""m4s  : scala.collection.immutable.Stream[Int] = """ + $show(m4s ));$skip(24); val res$0 = 
  (m4s take 100).toList;System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(97); 
  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0));System.out.println("""sieve: (s: Stream[Int])Stream[Int]""");$skip(35); val res$1 = 

  sieve(from(2)).take(100).toList;System.out.println("""res1: List[Int] = """ + $show(res$1))}
}
