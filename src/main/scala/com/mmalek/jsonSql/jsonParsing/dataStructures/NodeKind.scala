package com.mmalek.jsonSql.jsonParsing.dataStructures

import enumeratum._

sealed trait NodeKind extends EnumEntry

object NodeKind extends Enum[NodeKind] {
  val values: IndexedSeq[NodeKind] = findValues
  val functions: Set[String] = Set("sum", "avg", "median", "count", "max", "min")

  case object NoneNode extends NodeKind
  case class KeyNode(value: String) extends NodeKind
  case object ObjectNode extends NodeKind
  case object ArrayNode extends NodeKind
  case class ScalarNode(value: Any) extends NodeKind
}
