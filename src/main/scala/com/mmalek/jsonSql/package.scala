package com.mmalek

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JArray, JObject, JValue}
import com.mmalek.jsonSql.jsonParsing.getJson
import com.mmalek.jsonSql.sqlParsing.Token.{From, Json, Select, Where, Any => AnyToken}
import com.mmalek.jsonSql.sqlParsing.{Token, tokenize}

package object jsonSql {
  def runJsonSql(rawSql: String, rawJson: String): Option[Map[String, Seq[Option[JValue]]]] = {
    val json = getJson(rawJson)
    val tokens = tokenize(rawSql)
    val completeTokens = putJsonToken(tokens, json)

    if (completeTokens.head == Select) {
      val actions = getSelectionActions(completeTokens)

      if (completeTokens.contains(Where)) actions.from().flatMap(j => actions.where(j.value).map(actions.select))
      else actions.from().map(j => actions.select(j.value))
    } else None
  }

  private def putJsonToken(tokens: Seq[Token], json: JValue) =
    tokens.flatMap(t => if (t == From) List(t, Json(json)) else List(t))

  private def getSelectionActions(completeTokens: Seq[Token]) = {
    val actions = completeTokens.foldLeft(ActionsTuple(None, Map[Token, Seq[AnyToken]]()))(foldAsActionData).actions

    new {
      def select(json: JValue): Map[String, Seq[Option[JValue]]] =
        actions(Select).flatMap(getValues(_, json)).toMap

      def from(): Option[Json] =
        actions(From).head match {
          case token: Json => Some(token)
          case _ => None
        }

      def where(json: JValue): Option[JValue] =
        actions.get(Where).map(filterJson(_, json))
    }
  }

  private def foldAsActionData(aggregate: ActionsTuple, token: Token) = {
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
  }

  private def getValues(token: Token, value: JValue) =
    token match {
      case t: AnyToken => Some(t.value -> walkDown(t.value.split("\\."), value))
      case _ => None
    }

  private def filterJson(filters: Seq[Token], json: JValue): JValue =
    ???

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

  private case class ActionsTuple(currentToken: Option[Token],
                                  actions: Map[Token, Seq[Token]])
}
