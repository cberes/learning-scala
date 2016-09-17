package week4

import week3.{Cons, Nil}

object List {
  def apply[T](): week3.List[T] = Nil
  def apply[T](a: T): week3.List[T] = new Cons(a, List())
  def apply[T](a: T, b: T): week3.List[T] = new Cons(a, List(b))
}