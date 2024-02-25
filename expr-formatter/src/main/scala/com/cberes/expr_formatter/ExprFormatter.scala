package com.cberes.expr_formatter

import Element.elem

class ExprFormatter {
  private val opGroups = Array(
    Set("|", "||"),
    Set("&", "&&"),
    Set("^"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("*", "%")
  )

  private val precedence = {
    val assocs = for {
      i <- 0 until opGroups.length
      op <- opGroups(i)
    } yield op -> i
    assocs.toMap
  }

  private val unaryPrecedence = opGroups.length

  private val fractionPrecedence = -1

  def format(e : Expr) : Element = format(e, 0)

  private def format(e : Expr, enclosingPrecedence : Int) : Element = e match {
    case Var(name) => elem(name)

    case Number(num) =>
      def stripDot(s : String) =
        if (s endsWith ".0") s.substring(0, s.length - 2) else s
      elem(stripDot(num.toString))

    case UnaryOp(op, arg) =>
      elem(op) beside format(arg, unaryPrecedence)

    case BinaryOp("/", left, right) =>
      val top = format(left, fractionPrecedence)
      val bottom = format(right, fractionPrecedence)
      val line = elem('-', top.width max bottom.width, 1)
      val fraction = top above line above bottom
      if (enclosingPrecedence != fractionPrecedence) fraction
      else elem(" ") beside fraction beside elem(" ") // forces parent fraction to have a wider line

    case BinaryOp(op, left, right) =>
      val opPrecedence = precedence(op)
      val l = format(left, opPrecedence)
      val r = format(right, opPrecedence + 1)
      val expr = l beside elem(" ") beside elem(op) beside elem(" ") beside r
      if (enclosingPrecedence <= opPrecedence) expr
      else elem("(") beside expr beside elem(")")
  }
}
