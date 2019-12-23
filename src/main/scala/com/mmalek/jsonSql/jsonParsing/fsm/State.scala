package com.mmalek.jsonSql.jsonParsing.fsm

import enumeratum._

sealed trait State extends EnumEntry

object State extends Enum[State] {
  val values: IndexedSeq[State] = findValues

  case object Initial extends State
  case object ReadObject extends State
  case object ReadObjectEnd extends State
  case object ReadObjectKey extends State
  case object ReadArray extends State
  case object ReadArrayEnd extends State
  case object ReadScalar extends State
}