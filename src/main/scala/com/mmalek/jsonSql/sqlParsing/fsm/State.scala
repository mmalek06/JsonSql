package com.mmalek.jsonSql.sqlParsing.fsm

import enumeratum._

sealed trait State extends EnumEntry

object State extends Enum[State] {
  val values: IndexedSeq[State] = findValues

  case object Initial extends State
  case object ReadInsert extends State
  case object ReadSelect extends State
  case object ReadUpdate extends State
  case object ReadDelete extends State
  case object ReadFunction extends State
  case object ReadField extends State
  case object ReadConstant extends State
  case object ReadOperator extends State
  case object ReadFrom extends State
  case object ReadWhere extends State
}
