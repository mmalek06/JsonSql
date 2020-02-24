package com.mmalek.jsonSql.sqlParsing.fsm

import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.fsm.State._

import scala.collection.immutable.HashMap

class StateMachine(val state: State) {
  private val transitions: HashMap[State, Seq[(Char, String, Seq[State]) => Option[State]]] = HashMap(
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
      canReadConjunction,
      canReadAs),
    State.ReadConstant -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadFrom,
      canReadConjunction,
      canReadAs),
    State.ReadOperator -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadFrom -> Seq(
      canReadWhere),
    State.ReadWhere -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadConjunction -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant),
    State.ReadBracket -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadAs,
      canReadConjunction),
    State.ReadAs -> Seq(
      canReadAsValue),
    State.ReadAsValue -> Seq(
      canReadBracket,
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadFrom))
  private val operators = Set("-", "+", "/", "*", "%", "=", "!", ">", "<", "!=", ">=", "<=")

  def next(c: Char, sb: StringBuilder, history: Seq[State]): Option[StateMachine] = {
    val trimmedVal = sb.toString.trim
    val cleanValue =
      if (trimmedVal.startsWith(",")) trimmedVal.substring(1).trim
      else trimmedVal
    val transition = transitions(state).foldLeft(Option.empty[State])((aggregate, f) => aggregate match {
      case None => f(c, cleanValue, history)
      case x => x
    })

    transition match {
      case Some(value) => Some(new StateMachine(value))
      case _ => None
    }
  }

  private def canReadInsert(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "insert") Some(ReadInsert) else None

  private def canReadUpdate(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "update") Some(ReadUpdate) else None

  private def canReadDelete(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "delete") Some(ReadDelete) else None

  private def canReadSelect(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "select") Some(ReadSelect) else None

  private def canReadFunction(c: Char, valueSoFar: String, history: Seq[State]) = {
    val lowercaseValue = valueSoFar.toLowerCase

    if(valueSoFar.nonEmpty && (c == ' ' || c == '(') && Token.functions.contains(lowercaseValue)) Some(ReadFunction) else None
  }

  private def canReadField(c: Char, valueSoFar: String, history: Seq[State]) =
    if(valueSoFar.length > 2 && valueSoFar(0) == '"' && valueSoFar.last == '"') Some(ReadField) else None

  private def canReadConstant(c: Char, valueSoFar: String, history: Seq[State]) = {
    val cleanedValue = valueSoFar.replace("\\'", "'")

    if ((valueSoFar.nonEmpty && (valueSoFar.toBooleanOption.isDefined || valueSoFar.forall(_.isDigit)) && (c == ',' || c == ' ' || c == ')')) ||
        (cleanedValue.length > 2 && (cleanedValue(0) == '\'' && valueSoFar.takeRight(2) != "\\'" && cleanedValue.last == '\'')))
      Some(ReadConstant)
    else None
  }

  private def canReadOperator(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.nonEmpty && operators.contains(valueSoFar) && !operators.contains(s"${valueSoFar}$c")) Some(ReadOperator) else None

  private def canReadFrom(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "from") Some(ReadFrom) else None

  private def canReadWhere(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "where") Some(ReadWhere) else None

  private def canReadConjunction(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "and" | valueSoFar.toLowerCase == "or") Some(ReadConjunction) else None

  private def canReadBracket(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.length == 1 && (valueSoFar(0) == '(' || valueSoFar(0) == ')')) Some(ReadBracket) else None

  private def canReadAs(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == ' ' && valueSoFar.toLowerCase == "as") Some(ReadAs) else None

  private def canReadAsValue(c: Char, valueSoFar: String, history: Seq[State]) =
    if(history.headOption.contains(ReadAs) && valueSoFar.length > 2 && valueSoFar(0) == '"' && valueSoFar.last == '"') Some(ReadAsValue) else None
}
