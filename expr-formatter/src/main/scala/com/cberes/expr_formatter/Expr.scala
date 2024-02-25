package com.cberes.expr_formatter

sealed abstract class Expr
case class Var(name : String) extends Expr
case class Number(num : Double) extends Expr
case class UnaryOp(operator : String, arg : Expr) extends Expr
case class BinaryOp(operator : String, left : Expr, right : Expr) extends Expr
