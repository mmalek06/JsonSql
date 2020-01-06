package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import enumeratum._

sealed trait Token extends EnumEntry

object Token extends Enum[Token] {
  val values: IndexedSeq[Token] = findValues

  case object Sum extends Token
  case object Avg extends Token

  case object From extends Token

  case object Insert extends Token
  case object Select extends Token
  case object Update extends Token
  case object Delete extends Token

  case object Where extends Token

  case class Value(value: String) extends Token
  case class Json(value: JValue) extends Token
  case class Predicate(value: String) extends Token
}
