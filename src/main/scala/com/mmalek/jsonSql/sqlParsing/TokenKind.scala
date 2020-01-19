package com.mmalek.jsonSql.sqlParsing

import enumeratum._

sealed trait TokenKind extends EnumEntry

object TokenKind extends Enum[TokenKind] {
  val values: IndexedSeq[TokenKind] = findValues

  case object Function extends TokenKind
  case object Keyword extends TokenKind
  case object Other extends TokenKind
}


