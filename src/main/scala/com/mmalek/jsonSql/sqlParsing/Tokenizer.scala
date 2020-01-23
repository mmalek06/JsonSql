package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.sqlParsing.Token._
import com.mmalek.jsonSql.sqlParsing.fsm.State._
import com.mmalek.jsonSql.sqlParsing.fsm.{State, StateMachine}

import scala.annotation.tailrec

object Tokenizer {
  def tokenize(input: String): (Seq[Token], Option[String]) = {
    val cleanedInput = input.replace("##json##", "") + " "
    val seed = getSeed
    val ParsingTuple(_, tokens, _, error) = cleanedInput
      .foldLeft(seed)((aggregate, char) => {
        if (aggregate.invalidSql.isDefined) aggregate
        else
          aggregate.stateMachine.next(char, aggregate.builder).map(sm => {
            getToken(sm.state, aggregate.builder) match {
              case None => aggregate
              case Some(Left(error)) => aggregate.copy(invalidSql = Some(error))
              case Some(Right(token)) =>
                aggregate.builder.clear()
                aggregate.builder.append(char)
                aggregate.copy(stateMachine = sm, tokens = aggregate.tokens :+ token)
            }
          }).getOrElse({
            aggregate.builder.append(char)
            aggregate
          })
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
      case (_, Initial) => None
      case (_, ReadInsert) => Some(Right(Insert))
      case (_, ReadSelect) => Some(Right(Select))
      case (_, ReadUpdate) => Some(Right(Update))
      case (_, ReadDelete) => Some(Right(Delete))
      case (value, ReadFunction) => Some(getFunction(removeBrackets(value)))
      case (value, ReadField) => Some(Right(Field(removeBrackets(cleanValue(value)))))
      case (value, ReadConstant) => Some(Right(Constant(removeBrackets(cleanValue(value)))))
      case (value, ReadOperator) => Some(Right(Operator(removeBrackets(cleanValue(value))(0).toString)))
      case (_, ReadFrom) => Some(Right(From))
      case (_, ReadWhere) => Some(Right(Where))
      case (value, ReadConjunction) => Some(getConjunction(value))
      case (value, ReadBracket) => Some(getBracket(value))
    }

  private def getFunction(value: String) = {
    val cleanVal = cleanValue(value).toLowerCase

    Token.functions.find(t => t.name == cleanVal) match {
      case Some(t) => Right(t)
      case None => Left("This function is not implemented yet. Parsing aborted...")
    }
  }

  private def getConjunction(value: String) =
    cleanValue(value).toLowerCase match {
      case "and" => Right(And)
      case "or" => Right(Or)
      case _ => Left("This conjunction operator is not implemented yet. Parsing aborted...")
    }

  private def getBracket(value: String) =
    cleanValue(value) match {
      case "(" => Right(Bracket("("))
      case ")" => Right(Bracket(")"))
      case _ => Left("What kind of bracket is that?!")
    }

  private def removeBrackets(value: String): String = {
    val step1 =
      if (value(0) == '(') value.substring(1)
      else value
    val step2 =
      if (step1.last == ')') step1.substring(0, step1.length - 1)
      else step1

    step2
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
