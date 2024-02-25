package com.cberes.expr_formatter

import org.scalatest._

class ExprFormatterSpec extends FlatSpec with Matchers {
  "The formatter class" should "format expressions" in {
    val formatter = new ExprFormatter

    formatter.format(
      BinaryOp("+",
        BinaryOp("*",
          BinaryOp("+", Var("x"), Var("y")),
          Var("z")),
        Number(1))).toString shouldEqual "(x + y) * z + 1"

    formatter.format(
      BinaryOp("-", Var("a"), BinaryOp("-", Var("b"), Var("c")))).toString shouldEqual "a - (b - c)"

    formatter.format(
      BinaryOp("/", BinaryOp("/", Var("a"), Var("b")), Var("c"))).toString shouldEqual
      (" a \n" +
        " - \n" +
        " b \n" +
        "---\n" +
        " c ").stripMargin

    val e1 = BinaryOp("*", BinaryOp("/", Number(1), Number(2)),
                           BinaryOp("+", Var("x"), Number(1)))

    val e2 = BinaryOp("+", BinaryOp("/", Var("x"), Number(2)),
                           BinaryOp("/", Number(1.5), Var("x")))

    formatter.format(BinaryOp("/", e1, e2)).toString shouldEqual
      "1          \n" +
        "- * (x + 1)\n" +
        "2          \n" +
        "-----------\n" +
        "  x   1.5  \n" +
        "  - + ---  \n" +
        "  2    x   "
  }
}
