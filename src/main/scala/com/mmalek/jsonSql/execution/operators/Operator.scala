package com.mmalek.jsonSql.execution.operators

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token

trait Operator {
  def apply(json: JValue, tokens: Seq[Token]): Seq[Option[JValue]]
}
