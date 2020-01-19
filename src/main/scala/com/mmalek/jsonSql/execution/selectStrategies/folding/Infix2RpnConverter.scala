package com.mmalek.jsonSql.execution.selectStrategies.folding

import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Bracket, Constant, Field, Operator}
import com.mmalek.jsonSql.execution.StringOps._

import scala.collection.mutable

// Courtesy of: http://www.learn4master.com/data-structures/stack/convert-infix-notation-to-reverse-polish-notation-java
// sample imperative algorithm:
// Q = []
// S = []
//
// For each token in I:
//   if current_token:
//     is a number: add it to output Q
//     is '(' : Push it on to stack
//     is ')' :{ Pop items off stack to output Q until '(' reached and delete the '(' from the stack S. }
//     is an 'operator': {
//       while s.notEmpty() && precedence(s.peek()) >= precedence(current_token){
//         Pop Stack to output Q
//       }
//       push current token to Stack S
//     }
//
// After for loop: Finally pop all the elements in Stack S to output Q.
//
// It can be made functional, and Ima do it if I have time, I promise!
object Infix2RpnConverter {
  def convert(eqn: Seq[Token]): Seq[Token] = {
    val prededence = Map[String, Int](
      "/" -> 5,
      "*" -> 5,
      "%" -> 5,
      "+" -> 4,
      "-" -> 4,
      "(" -> 0)
    val queue = mutable.Queue[Token]()
    val operators = mutable.Stack[Token]()

    for (currentToken <- eqn) {
      currentToken match {
        case Field(_) | Constant(_) => queue.addOne(currentToken)
        case x: Token if Token.functions.contains(x) => queue.addOne(currentToken)
        case x: Bracket if x.isOpening => operators.push(currentToken)
        case x: Bracket if !x.isOpening =>
          queue.addAll(operators.popWhile {
            case y: Bracket if y.isOpening => true
            case _ => false
          })
        case Operator(value) =>
          operators.popWhile {
            case Operator(op) => prededence(op) >= prededence(value)
            case x: Bracket if x.isOpening => prededence(x.value) >= prededence(value)
            case _ => false
          }
          operators.push(currentToken)
      }
    }

    queue.addAll(operators.popAll())

    Nil
  }
}
