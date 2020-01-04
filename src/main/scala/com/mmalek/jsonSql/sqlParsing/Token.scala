package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.TokenType.{Function, Initializer, Statement, Value}
import enumeratum._

sealed abstract class Token(`type`: TokenType) extends EnumEntry

object Token extends Enum[Token] {
  val values: IndexedSeq[Token] = findValues

  case object Sum extends Token(Function)
  case object Avg extends Token(Function)

  case object From extends Token(Initializer)

  case object Insert extends Token(Statement)
  case object Select extends Token(Statement)
  case object Update extends Token(Statement)
  case object Delete extends Token(Statement)

  case object Where extends Token(Statement)

  case class Any(value: String) extends Token(Value)
  case class Json(value: JValue) extends Token(Value)
}
