package com.mmalek.jsonSql

import com.mmalek.jsonSql.execution.rpn.{Infix2RpnArithmeticConverter, Infix2RpnLogicalConverter}
import com.mmalek.jsonSql.sqlParsing.Token._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InfixConvertersTests extends AnyFlatSpec with Matchers {
  "Arithmetic converter" should "reorder tokens in simple expression" in {
    val tokens = Seq(
      Constant("5"),
      Operator("+"),
      Constant("7"))
    val result = Infix2RpnArithmeticConverter.convert(tokens)

    result.length should be (3)
    result.head should be (Constant("5"))
    result(1) should be (Constant("7"))
    result(2) should be (Operator("+"))
  }

  it should "reorder tokens in complex expression" in {
    val tokens = Seq(
      Bracket("("),
      Bracket("("),
      Constant("5"),
      Operator("+"),
      Constant("7"),
      Bracket(")"),
      Operator("*"),
      Constant("2"),
      Operator("+"),
      Constant("9"),
      Bracket(")"),
      Operator("*"),
      Bracket("("),
      Constant("8"),
      Operator("/"),
      Constant("2"),
      Bracket(")"))
    val result = Infix2RpnArithmeticConverter.convert(tokens)

    result.length should be (11)
    result.head should be (Constant("5"))
    result(1) should be (Constant("7"))
    result(2) should be (Operator("+"))
    result(3) should be (Constant("2"))
    result(4) should be (Operator("*"))
    result(5) should be (Constant("9"))
    result(6) should be (Operator("+"))
    result(7) should be (Constant("8"))
    result(8) should be (Constant("2"))
    result(9) should be (Operator("/"))
    result(10) should be (Operator("*"))
  }

  "Logical converter" should "reorder tokens in simple expression" in {
    val tokens = Seq(
      Constant("true"),
      Operator("OR"),
      Constant("false"))
    val result = Infix2RpnArithmeticConverter.convert(tokens)

    result.length should be (3)
    result.head should be (Constant("true"))
    result(1) should be (Constant("false"))
    result(2) should be (Operator("OR"))
  }

  it should "reorder tokens in complex expression" in {
    val tokens = Seq(
      Constant("true"),
      Or,
      Bracket("("),
      Constant("true"),
      And,
      Constant("true"),
      Bracket(")"),
      And,
      Constant("true"))
    val result = Infix2RpnLogicalConverter.convert(tokens)

    result.length should be (7)
    result.head should be (Constant("true"))
    result(1) should be (Constant("true"))
    result(2) should be (Constant("true"))
    result(3) should be (And)
    result(4) should be (Or)
    result(5) should be (Constant("true"))
    result(6) should be (And)
  }
}
