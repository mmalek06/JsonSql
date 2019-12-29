package com.mmalek.jsonSql.jsonParsing.dataStructures

sealed trait NodeKind

case class KeyNode(value: String) extends NodeKind
case object ObjectNode extends NodeKind
case object ArrayNode extends NodeKind
case class ScalarNode(value: Any) extends NodeKind