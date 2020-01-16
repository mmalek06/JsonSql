package com.mmalek.jsonSql.execution.selectStrategies

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token

object FoldingStrategy {
  def run(tokens: Seq[Token], json: JValue): Map[String, Seq[Option[JValue]]] =
    ???
}
