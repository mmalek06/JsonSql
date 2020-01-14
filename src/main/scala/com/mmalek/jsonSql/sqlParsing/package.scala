package com.mmalek.jsonSql

import com.mmalek.jsonSql.sqlParsing.Token._
import com.mmalek.jsonSql.sqlParsing.fsm.State._
import com.mmalek.jsonSql.sqlParsing.fsm.{State, StateMachine}

import scala.annotation.tailrec

package object sqlParsing {
  def tokenize(input: String): (Seq[Token], Option[String]) = {
    val cleanedInput = input.replace("##json##", "")
    val seed = getSeed
    val ParsingTuple(_, tokens, _, error) = cleanedInput
      .foldLeft(seed)((aggregate, char) => {
        if (aggregate.invalidSql.isDefined) aggregate
        else {
          aggregate.stateMachine.next(char, aggregate.builder).map(sm => {
            getToken(sm.state, aggregate.builder) match {
              case Left(error) => aggregate.copy(invalidSql = Some(error))
              case Right(token) =>
                aggregate.builder.clear()
                aggregate.builder.append(char)
                aggregate.copy(stateMachine = sm, tokens = aggregate.tokens :+ token)
            }
          }).getOrElse({
            aggregate.builder.append(char)
            aggregate
          })
        }
      })

    (tokens, error)
  }

  private def getSeed =
    ParsingTuple(
      new StateMachine(State.Initial),
      List.empty[Token],
      new StringBuilder,
      None)

  private def getToken(state: State, builder: StringBuilder) =
    (builder.toString.trim, state) match {
      case (_, ReadInsert) => Right(Insert)
      case (_, ReadSelect) => Right(Select)
      case (_, ReadUpdate) => Right(Update)
      case (_, ReadDelete) => Right(Delete)
      case (value, ReadFunction) => getFunction(value)
      case (value, ReadField) => Right(Field(cleanValue(value)))
      case (value, ReadConstant) => Right(Constant(cleanValue(value)))
      case (value, ReadOperator) => Right(Operator(cleanValue(value)(0).toString))
      case (_, ReadFrom) => Right(From)
      case (_, ReadWhere) => Right(Where)
    }

  private def getFunction(value: String) =
    cleanValue(value).toLowerCase match {
      case "avg" => Right(Avg)
      case "sum" => Right(Sum)
      case _ => Left("This function is not implemented yet. Parsing aborted...")
    }

  @tailrec
  private def cleanValue(value: String): String = {
    val trimmed = value.trim

    if (trimmed.nonEmpty && trimmed(0) == ',') cleanValue(trimmed.substring(1))
    else trimmed
  }

  private case class ParsingTuple(stateMachine: StateMachine,
                                  tokens: Seq[Token],
                                  builder: StringBuilder,
                                  invalidSql: Option[String])
}
