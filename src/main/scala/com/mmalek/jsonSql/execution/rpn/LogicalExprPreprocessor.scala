package com.mmalek.jsonSql.execution.rpn

import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{And, Bracket, Or}

object LogicalExprPreprocessor {
  def apply(expr: Seq[Token]): Seq[Token] =
    addBrackets(expr)

  private def addBrackets(expr: Seq[Token]): Seq[Token] =
    Seq(Bracket("(")) ++ Seq(Bracket("(")) ++ expr.flatMap(t => t match {
        case And => Seq(Bracket(")"), t, Bracket("("))
        case Or => Seq(Bracket(")"), Bracket(")"), t, Bracket("("), Bracket("("))
        case _ => Seq(t)
      }) ++ Seq(Bracket(")")) ++ Seq(Bracket(")"))
}
