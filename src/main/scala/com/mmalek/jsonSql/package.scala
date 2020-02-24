package com.mmalek

import com.mmalek.jsonSql.execution.SelectExecutor
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.jsonParsing.JsonParser
import com.mmalek.jsonSql.sqlParsing.Token._
import com.mmalek.jsonSql.sqlParsing.{Token, Tokenizer}

package object jsonSql {
  def runQuery(rawSql: String, rawJson: String): Either[String, Map[String, Seq[Option[JValue]]]] = {
    val json = JsonParser.getJson(rawJson)
    val (tokens, error) = Tokenizer.tokenize(rawSql)

    error match {
      case Some(e) => Left(e)
      case None =>
        val completeTokens = putJsonToken(tokens, json)

        if (completeTokens.head == Select) {
          val actions = getSelectionActions(completeTokens)

          actions.from() match {
            case Right(from) =>
              if (completeTokens.contains(Where)) actions.where(from.value) match {
                case Right(value) => actions.select(value)
                case Left(error) => Left(error)
              }
              else actions.select(from.value)
            case Left(error) => Left(error)
          }
        } else Right(Map.empty[String, Seq[Option[JValue]]])
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
