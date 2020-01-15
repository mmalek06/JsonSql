package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import enumeratum._

sealed abstract class Token(val name: String = "") extends EnumEntry

object Token extends Enum[Token] {
  val values: IndexedSeq[Token] = findValues

  case object Sum extends Token("sum")
  case object Avg extends Token("avg")

  case object From extends Token

  case object Insert extends Token
  case object Select extends Token
  case object Update extends Token
  case object Delete extends Token

  case object Where extends Token
  case object And extends Token
  case object Or extends Token

  case class Json(value: JValue) extends Token
  case class Field(value: String) extends Token
  case class Constant(value: String) extends Token
  case class Operator(value: String) extends Token
}
