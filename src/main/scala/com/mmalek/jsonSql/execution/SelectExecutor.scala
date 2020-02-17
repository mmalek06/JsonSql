package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.filtering.{FoldingStrategy => FilteringFoldingStrategy}
import com.mmalek.jsonSql.execution.selection.{MappingStrategy, FoldingStrategy => SelectionFoldingStrategy}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._

class SelectExecutor(actions: Map[Token, Seq[Token]]) {
  def select(json: JValue): Either[String, Map[String, Seq[Option[JValue]]]] = {
    val tokens = actions(Select)
    val info = getSelectionInfo(tokens)

    if (info.hasFunctions || info.hasOperators) SelectionFoldingStrategy(tokens, json)
    else MappingStrategy(json, tokens)
  }

  def from(): Option[Json] =
    actions(From).head match {
      case token: Json => Some(token)
      case _ => None
    }

  def where(json: JValue): JValue =
    actions.get(Where) match {
      case Some(value) if value.nonEmpty => FilteringFoldingStrategy(value, json)
      case _ => json
    }

  private def getSelectionInfo(tokens: Seq[Token]) =
    tokens.foldLeft(SelectionTokensInfo(hasOperators = false, hasFunctions = false))((aggregate, t) => t match {
      case _: Operator => aggregate.copy(hasOperators = true)
      case Token.Function(_) => aggregate.copy(hasFunctions = true)
      case _ => aggregate
    })

  case class SelectionTokensInfo(hasOperators: Boolean, hasFunctions: Boolean)
}
