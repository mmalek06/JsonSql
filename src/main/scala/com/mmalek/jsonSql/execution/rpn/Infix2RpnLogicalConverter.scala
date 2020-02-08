package com.mmalek.jsonSql.execution.rpn

import cats.implicits._
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._

import scala.collection.mutable

object Infix2RpnLogicalConverter {
  private val functionsPrecedence = Token.functions.map(t => t -> 4).toSeq.toMap
  private val operatorsPrecedence = Map("=" -> 3, ">" -> 3, "<" -> 3, "!=" -> 3, "or" -> 1, "and" -> 2, "(" -> 0)
  private val precedence = operatorsPrecedence.combine(functionsPrecedence)

  def convert(expr: Seq[Token]): Seq[Token] = {
    val queue = mutable.Queue[Token]()
    val operators = mutable.Stack[Token]()
    val preprocessedExpr = LogicalExprPreprocessor(expr)

    for (currentToken <- preprocessedExpr) {
      currentToken match {
        case Field(_) | Constant(_) => queue.addOne(currentToken)
        case x: Bracket if x.isOpening => operators.push(currentToken)
        case x: Bracket if !x.isOpening => handleBracket(queue, operators)
        case And => handleAnd(queue, operators, currentToken)
        case Or => handleOr(queue, operators, currentToken)
        case Operator(value) => handleOperators(operators, queue, currentToken, value)
        case Token.Function(name) => handleOperators(operators, queue, currentToken, name)
      }
    }

    queue.addAll(operators.popAll).toSeq
  }

  private def handleBracket(queue: mutable.Queue[Token], operators: mutable.Stack[Token]) = {
    queue.addAll(operators.popWhile {
      case y: Bracket if y.isOpening => false
      case _ => true
    })
    operators.popWhile {
      case y: Bracket if y.isOpening => true
      case _ => false
    }
  }

  private def handleAnd(queue: mutable.Queue[Token], operators: mutable.Stack[Token], currentToken: Token) = {
    queue.addAll(operators.popWhile {
      case Or => false
      case _ => true
    })
    operators.push(currentToken)
  }

  private def handleOr(queue: mutable.Queue[Token], operators: mutable.Stack[Token], currentToken: Token) = {
    queue.addAll(operators.popWhile {
      case Or => true
      case _ => false
    })
    operators.push(currentToken)
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
