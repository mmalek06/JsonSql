package com.mmalek.jsonSql.execution.selection

import com.mmalek.jsonSql.extensions.JValueOps._
import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field, FieldAlias}

object MappingStrategy {
  def apply(json: JValue, tokens: Seq[Token]): Either[String, Map[String, Seq[Option[JValue]]]] = {
    val rawValues = tokens.flatMap(getValues(json, _)).toMap
    var pvalues = tokens.flatMap(x(json, _))
    val aliases = findAliases(tokens)
    val aliasedValues = rawValues.map(pair => aliases.get(pair._1).map(alias => (alias, pair._2)).getOrElse(pair))

    Right(aliasedValues)
  }

  private def getValues(value: JValue, token: Token) =
    token match {
      case t: Field => Some(t.value -> value.getValues(t.value.split("\\.")))
      case t: Constant => Some(t.value -> Seq(Some(t.value.asJValue)))
      case _ => None
    }

  private def x(value: JValue, token: Token) =
    token match {
      case t: Field => Some(t.value -> value.getParentedValues(t.value.split("\\.")))
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
    }).filter(_._2 != "").toMap
}
