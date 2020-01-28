package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import enumeratum._

sealed abstract class Token() extends EnumEntry

object Token extends Enum[Token] {
  val values: IndexedSeq[Token] = findValues
  val functions: Set[String] = Set("sum", "avg", "median", "count", "max", "min")

  case object Default extends Token

  case class Function(name: String) extends Token()

  case object From extends Token()
  case object Insert extends Token()
  case object Select extends Token()
  case object Update extends Token()
  case object Delete extends Token()
  case object Where extends Token()
  case object As extends Token()
  case object And extends Token()
  case object Or extends Token()

  case class Json(value: JValue) extends Token()
  case class Bracket(value: String) extends Token() {
    val isOpening: Boolean = value == "("
  }
  case class Field(value: String) extends Token()
  case class Constant(value: String) extends Token()
  case class Operator(value: String) extends Token()
}
