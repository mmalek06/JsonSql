package com.mmalek.jsonSql.execution.selectStrategies

import com.mmalek.jsonSql.jsonParsing.StringExtensions._
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JArray, JObject, JValue}
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}

object MappingStrategy {
  def run(tokens: Seq[Token], json: JValue): Map[String, Seq[Option[JValue]]] =
    tokens.flatMap(getValues(_, json)).toMap

  private def getValues(token: Token, value: JValue): Option[(String, Seq[Option[JValue]])] =
    token match {
      case t: Field => Some(t.value -> walkDown(t.value.split("\\."), value))
      case t: Constant => Some(t.value -> Seq(Some(t.value.asJValue)))
      case _ => None
    }

  private def walkDown(path: Seq[String], json: JValue): Seq[Option[JValue]] =
    if (path.isEmpty) Seq(None)
    else json match {
      case JObject(obj) =>
        obj.find(_.name == path.head).map(_.value) match {
          case Some(value) if path.tail.isEmpty => Seq(Some(value))
          case Some(value) => walkDown(path.tail, value)
          case _ => Seq(None)
        }
      case JArray(arr) if path.tail.isEmpty => Seq(Some(JArray(arr)))
      case JArray(arr) => arr.flatMap(v => walkDown(path.tail, v))
      case _ => Seq(Some(json))
    }
}
