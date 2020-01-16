package com.mmalek

import com.mmalek.jsonSql.execution.SelectExecutor
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.jsonParsing.getJson
import com.mmalek.jsonSql.sqlParsing.Token._
import com.mmalek.jsonSql.sqlParsing.{Token, tokenize}

package object jsonSql {
  def runJsonSql(rawSql: String, rawJson: String): Either[String, Option[Map[String, Seq[Option[JValue]]]]] = {
    val json = getJson(rawJson)
    val (tokens, error) = tokenize(rawSql)

    error match {
      case Some(e) => Left(e)
      case None =>
        val completeTokens = putJsonToken(tokens, json)

        if (completeTokens.head == Select) {
          val actions = getSelectionActions(completeTokens)

          if (completeTokens.contains(Where)) Right(actions.from().flatMap(j => actions.where(j.value).map(actions.select)))
          else Right(actions.from().map(j => actions.select(j.value)))
        } else Right(None)
    }
  }

  private def putJsonToken(tokens: Seq[Token], json: JValue) =
    tokens.flatMap(t => if (t == From) List(t, Json(json)) else List(t))

  private def getSelectionActions(completeTokens: Seq[Token]) =
    new SelectExecutor(
      completeTokens
        .foldLeft(ActionsTuple(None, Map[Token, Seq[Token]]()))(foldAsActionData)
        .actions)

  private def foldAsActionData(aggregate: ActionsTuple, token: Token) =
    token match {
      case Select => ActionsTuple(Some(Select), aggregate.actions ++ Map(Select -> Nil))
      case From => ActionsTuple(Some(From), aggregate.actions ++ Map(From -> Nil))
      case Where => ActionsTuple(Some(Where), aggregate.actions ++ Map(Where -> Nil))
      case x => aggregate.currentToken.map(token => {
        val nextArgs = aggregate.actions(token) :+ x
        val nextActions = aggregate.actions ++ Map(token -> nextArgs)

        aggregate.copy(actions = nextActions)
      }).get
    }

  private case class ActionsTuple(currentToken: Option[Token],
                                  actions: Map[Token, Seq[Token]])
}
