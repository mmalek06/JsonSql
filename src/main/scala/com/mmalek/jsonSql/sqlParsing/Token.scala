package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import enumeratum._

sealed abstract class Token(val name: String = "") extends EnumEntry

object Token extends Enum[Token] {
  val values: IndexedSeq[Token] = findValues

  case class Sum() extends Token("sum")
  case class Avg() extends Token("avg")
  case class Median() extends Token("median")
  case class Count() extends Token("count")
  case class Max() extends Token("max")
  case class Min() extends Token("min")

  case object From extends Token

  case object Insert extends Token
  case object Select extends Token
  case object Update extends Token
  case object Delete extends Token

  case object Where extends Token
  case object And extends Token
  case object Or extends Token

  case class Json(value: JValue) extends Token
  case class Bracket(value: Char) extends Token {
    val isOpening: Boolean = value == '('
  }
  case class Field(value: String) extends Token
  case class Constant(value: String) extends Token
  case class Operator(value: String) extends Token
}
