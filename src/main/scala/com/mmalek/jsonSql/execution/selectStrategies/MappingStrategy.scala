package com.mmalek.jsonSql.execution.selectStrategies

import com.mmalek.jsonSql.execution.JValueOps._
import com.mmalek.jsonSql.jsonParsing.StringOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}

object MappingStrategy {
  def apply(tokens: Seq[Token], json: JValue): Map[String, Seq[Option[JValue]]] =
    tokens.flatMap(getValues(_, json)).toMap

  private def getValues(token: Token, value: JValue): Option[(String, Seq[Option[JValue]])] =
    token match {
      case t: Field => Some(t.value -> value.getValuesFor(t.value.split("\\.")))
      case t: Constant => Some(t.value -> Seq(Some(t.value.asJValue)))
      case _ => None
    }
}
