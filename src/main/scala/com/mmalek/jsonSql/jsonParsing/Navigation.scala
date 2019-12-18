package com.mmalek.jsonSql.jsonParsing

import enumeratum._

sealed trait Navigation extends EnumEntry

object Navigation extends Enum[Navigation] {
  val values: IndexedSeq[Navigation] = findValues

  case object Up extends Navigation
  case object Down extends Navigation
  case object Stay extends Navigation
}
