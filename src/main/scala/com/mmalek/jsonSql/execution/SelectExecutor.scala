package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.selection.{MappingStrategy, FoldingStrategy => SelectionFoldingStrategy}
import com.mmalek.jsonSql.execution.{Filter => DoFilter}
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

  def from(): Either[String, Json] =
    actions.get(From) match {
      case Some((h: Json) :: _) => Right(h)
      case _ => Left("FROM statement not present. The cause for this error may be that you forgot to alias function call in preceding SELECT clause.")
    }

  def where(json: JValue): Either[String, JValue] =
    actions.get(Where) match {
      case Some(value) if value.nonEmpty => DoFilter(value, json)
      case _ => Right(json)
    }

  private def getSelectionInfo(tokens: Seq[Token]) =
    tokens.foldLeft(SelectionTokensInfo(hasOperators = false, hasFunctions = false))((aggregate, t) => t match {
      case _: Operator => aggregate.copy(hasOperators = true)
      case Token.Function(_) => aggregate.copy(hasFunctions = true)
      case _ => aggregate
    })

  case class SelectionTokensInfo(hasOperators: Boolean, hasFunctions: Boolean)
}
