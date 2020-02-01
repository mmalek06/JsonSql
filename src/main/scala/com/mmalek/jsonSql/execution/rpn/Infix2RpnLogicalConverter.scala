package com.mmalek.jsonSql.execution.rpn

import cats.implicits._
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._

import scala.collection.mutable

object Infix2RpnLogicalConverter {
  private val functionsPrecedence = Token.functions.map(t => t -> 3).toSeq.toMap
  private val operatorsPrecedence = Map("=" -> 2, ">" -> 2, "<" -> 2, "!=" -> 2, "or" -> 1, "and" -> 1, "(" -> 0)
  private val precedence = operatorsPrecedence.combine(functionsPrecedence)

  def convert(expr: Seq[Token]): Seq[Token] = {
    val queue = mutable.Queue[Token]()
    val operators = mutable.Stack[Token]()
    val preprocessedExpr = addBrackets(expr)

    for (currentToken <- preprocessedExpr) {
      currentToken match {
        case Field(_) | Constant(_) => queue.addOne(currentToken)
        case x: Bracket if x.isOpening => operators.push(currentToken)
        case x: Bracket if !x.isOpening => handleBracket(queue, operators)
        case And | Or => handleConjunctions(queue, operators, currentToken)
        case Operator(value) => handleOperators(operators, queue, currentToken, value)
        case Token.Function(name) => handleOperators(operators, queue, currentToken, name)
      }
    }

    queue.addAll(operators.popAll).toSeq
  }

  private def addBrackets(expr: Seq[Token]) =
    Seq(Bracket("(")) ++
    expr.flatMap(t => t match {
      case And | Or => Seq(Bracket(")"), t, Bracket("("))
      case _ => Seq(t)
    }) ++
    Seq(Bracket(")"))

  private def handleBracket(queue: mutable.Queue[Token], operators: mutable.Stack[Token]) = {
    queue.addAll(operators.popWhile {
      case y: Bracket if y.isOpening => false
      case _ => true
    })
    operators.pop()
  }

  private def handleConjunctions(queue: mutable.Queue[Token], operators: mutable.Stack[Token], currentToken: Token) = {
    queue.addAll(operators.popAll)
    operators.addOne(currentToken)
  }

  private def handleOperators(operators: mutable.Stack[Token], queue: mutable.Queue[Token], currentToken: Token, value: String) = {
    queue.addAll(operators.popWhile {
      case Operator(op) => precedence(op) >= precedence(value)
      case Token.Function(name) => precedence(name) >= precedence(value)
      case x: Bracket if x.isOpening => precedence(x.value) >= precedence(value)
      case _ => false
    })
    operators.push(currentToken)
  }
}
