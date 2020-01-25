package com.mmalek.jsonSql.sqlParsing.fsm

import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.fsm.State._

import scala.collection.immutable.HashMap

class StateMachine(val state: State) {
  private val transitions: HashMap[State, Seq[(Char, String) => Option[State]]] = HashMap(
    State.Initial -> Seq(
      canReadInsert,
      canReadSelect,
      canReadUpdate,
      canReadDelete),
    State.ReadSelect -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadFunction -> Seq(
      canReadBracket,
      canReadField),
    State.ReadField -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadFrom,
      canReadConjunction),
    State.ReadConstant -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadFrom,
      canReadConjunction),
    State.ReadOperator -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadFrom -> Seq(
      canReadWhere),
    State.ReadWhere -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadConjunction -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadBracket -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator))
  private val operators = Set('-', '+', '/', '*', '%', '=', '!')

  def next(c: Char, sb: StringBuilder): Option[StateMachine] = {
    val trimmedVal = sb.toString.trim
    val cleanValue =
      if (trimmedVal.startsWith(",")) trimmedVal.substring(1).trim
      else trimmedVal
    val transition = transitions(state).foldLeft(Option.empty[State])((aggregate, f) => aggregate match {
      case None => f(c, cleanValue)
      case x => x
    })

    transition match {
      case Some(value) => Some(new StateMachine(value))
      case _ => None
    }
  }

  private def canReadInsert(c: Char, valueSoFar: String) =
    if (valueSoFar.toLowerCase == "insert") Some(ReadInsert) else None

  private def canReadUpdate(c: Char, valueSoFar: String) =
    if (valueSoFar.toLowerCase == "update") Some(ReadUpdate) else None

  private def canReadDelete(c: Char, valueSoFar: String) =
    if (valueSoFar.toLowerCase == "delete") Some(ReadDelete) else None

  private def canReadSelect(c: Char, valueSoFar: String) =
    if (valueSoFar.toLowerCase == "select") Some(ReadSelect) else None

  private def canReadFunction(c: Char, valueSoFar: String) = {
    val lowercasedValue = valueSoFar.toLowerCase

    if(valueSoFar.nonEmpty && (c == ' ' || c == '(' || operators.contains(c)) && Token.functions.contains(lowercasedValue)) Some(ReadFunction) else None
  }

  private def canReadField(c: Char, valueSoFar: String) =
    if(valueSoFar.length > 2 && valueSoFar(0) == '"' && valueSoFar.last == '"') Some(ReadField) else None

  private def canReadConstant(c: Char, valueSoFar: String) =
    if ((valueSoFar.nonEmpty && (valueSoFar.toBooleanOption.isDefined || valueSoFar.forall(_.isDigit))) ||
        (valueSoFar.length > 2 && (valueSoFar(0) == '\'' && valueSoFar.last == '\''))) Some(ReadConstant)
    else None

  private def canReadOperator(c: Char, valueSoFar: String) =
    if (valueSoFar.nonEmpty && operators.contains(valueSoFar(0))) Some(ReadOperator) else None

  private def canReadFrom(c: Char, valueSoFar: String) =
    if (valueSoFar.toLowerCase == "from") Some(ReadFrom) else None

  private def canReadWhere(c: Char, valueSoFar: String) =
    if (valueSoFar.toLowerCase == "where") Some(ReadWhere) else None

  private def canReadConjunction(c: Char, valuesSoFar: String) =
    if (valuesSoFar.toLowerCase == "and" | valuesSoFar.toLowerCase == "or") Some(ReadConjunction) else None

  private def canReadBracket(c: Char, valuesSoFar: String) =
    if (valuesSoFar.length == 1 && (valuesSoFar(0) == '(' || valuesSoFar(0) == ')')) Some(ReadBracket) else None
}
