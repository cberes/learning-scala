package week4

trait Expr {
  type Environment = String => Int

  def eval(env: Environment): Int = this match {
    case Number(n) => n
    case Var(x) => env(x)
    case Sum(a, b) => a.eval(env) + b.eval(env)
    case Prod(a, b) => a.eval(env) * b.eval(env)
  }

  override def toString: String = this match {
    case Number(n) => n.toString
    case Var(name) => name
    case Sum(a, b) => a + " + " + b
    case Prod(a: Sum, b: Sum) => "(" + a + ") * (" + b + ")"
    case Prod(a: Sum, b) => "(" + a + ") * " + b
    case Prod(a, b: Sum) => a + " * (" + b + ")"
    case Prod(a, b) => a + " * " + b
  }
}

case class Number(n: Int) extends Expr
case class Sum(a: Expr, b: Expr) extends Expr
case class Var(name: String) extends Expr
case class Prod(a: Expr, b: Expr) extends Expr

object ExprMain extends App {
  println(Sum(Number(1), Number(2)).eval({case "x" => 2}))
  println(Sum(Number(1), Number(2)))
  // 2 * x + y
  println(Sum(Prod(Number(2), Var("x")), Var("y")))
  // (2 + x) * y
  println(Prod(Sum(Number(2), Var("x")), Var("y")))
  println(Prod(Sum(Number(2), Var("x")), Var("y")).eval({case "x" => 2 case "y" => 3}))
}