package com.mmalek.jsonSql.sqlParsing.tokens

import enumeratum._

sealed abstract class TokenType extends EnumEntry

object TokenType extends Enum[TokenType] {
  val values: IndexedSeq[TokenType] = findValues

  case object Initializer extends TokenType
  case object Function extends TokenType
  case object Statement extends TokenType
  case object Value extends TokenType
}
