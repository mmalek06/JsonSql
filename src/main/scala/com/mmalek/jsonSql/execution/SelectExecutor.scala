package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.selectStrategies.{FoldingStrategy, MappingStrategy}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._

class SelectExecutor(actions: Map[Token, Seq[Token]]) {
  def select(json: JValue): Map[String, Seq[Option[JValue]]] = {
    val tokens = actions(Select)

    if (willFold(tokens)) FoldingStrategy.run(tokens, json)
    else MappingStrategy.run(tokens, json)
  }

  def from(): Option[Json] =
    actions(From).head match {
      case token: Json => Some(token)
      case _ => None
    }

  def where(json: JValue): Option[JValue] =
    actions.get(Where).map(filterJson(_, json))

  private def willFold(tokens: Seq[Token]) =
    tokens.exists(t => t match {
      case _: Operator | _: Avg | _: Sum => true
      case _ => false
    })

  private def filterJson(filters: Seq[Token], json: JValue): JValue =
    filters.flatMap {
      case t: Field => Some(t)
      case t: Constant => Some(t)
      case t: Operator => Some(t)
      case _ => None
    } match {
      case Nil => json
      case _ => json
    }
}
