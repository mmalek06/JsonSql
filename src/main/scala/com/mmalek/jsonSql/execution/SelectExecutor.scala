package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.selectStrategies.MappingStrategy
import com.mmalek.jsonSql.execution.selectStrategies.folding.FoldingStrategy
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._

class SelectExecutor(actions: Map[Token, Seq[Token]]) {
  def select(json: JValue): Map[String, Seq[Option[JValue]]] = {
    val tokens = actions(Select)
    val info = getTokensInfo(tokens)

    if (info.hasFunctions || info.hasOperators) FoldingStrategy(tokens, json, info)
    else MappingStrategy(tokens, json)
  }

  def from(): Option[Json] =
    actions(From).head match {
      case token: Json => Some(token)
      case _ => None
    }

  def where(json: JValue): Option[JValue] =
    actions.get(Where).map(filterJson(_, json))

  private def getTokensInfo(tokens: Seq[Token]) =
    tokens.foldLeft(TokensInfo(hasOperators = false, hasFunctions = false))((aggregate, t) => t match {
      case _: Operator => aggregate.copy(hasOperators = true)
      case x if Token.functions.contains(x.name) => aggregate.copy(hasFunctions = true)
      case _ => aggregate
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
