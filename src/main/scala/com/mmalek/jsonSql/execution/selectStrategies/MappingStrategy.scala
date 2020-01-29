package com.mmalek.jsonSql.execution.selectStrategies

import com.mmalek.jsonSql.extensions.JValueOps._
import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field, FieldAlias}

object MappingStrategy {
  def apply(tokens: Seq[Token], json: JValue): Either[String, Map[String, Seq[Option[JValue]]]] = {
    val rawValues = tokens.flatMap(getValues(_, json)).toMap
    val aliases = findAliases(tokens)

    Right(rawValues)
  }

  private def getValues(token: Token, value: JValue): Option[(String, Seq[Option[JValue]])] =
    token match {
      case t: Field => Some(t.value -> value.getValuesFor(t.value.split("\\.")))
      case t: Constant => Some(t.value -> Seq(Some(t.value.asJValue)))
      case _ => None
    }

  private def findAliases(tokens: Seq[Token]) =
    tokens.foldLeft(Seq.empty[(String, String)])((aggregate, t) => t match {
      case Field(name) => aggregate :+ (name, "")
      case Constant(value) => aggregate :+ (value, "")
      case FieldAlias(alias) =>
        val (key, _) = aggregate.last

        aggregate.init :+ (key, alias)
      case _ => aggregate
    }).filter(_._2 != "")
}
