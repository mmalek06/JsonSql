package com.mmalek.jsonSql.sqlParsing.fsm

import com.mmalek.jsonSql.sqlParsing.Token.{Avg, Sum}
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
      canReadFunction,
      canReadField,
      canReadConstant
    ),
    State.ReadFunction -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadFrom
    ),
    State.ReadField -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadFrom
    ),
    State.ReadConstant -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant,
      canReadOperator,
      canReadFrom
    ),
    State.ReadOperator -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant
    ),
    State.ReadFrom -> Seq(
      canReadWhere
    ),
    State.ReadWhere -> Seq(
      canReadFunction,
      canReadField,
      canReadConstant
    ))
  private val functions = Set(Sum.name, Avg.name)
  private val operators = Set('-', '+', '/', '*', '%', '=')

  def next(c: Char, sb: StringBuilder, history: Seq[State]): Option[StateMachine] =
    transitions(state).flatMap(f => f(c, sb.toString.trim, history)) match {
      case x :: Nil => Some(new StateMachine(x))
      case _ => None
    }

  private def canReadInsert(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "insert") Some(ReadSelect) else None

  private def canReadUpdate(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "update") Some(ReadSelect) else None

  private def canReadDelete(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "delete") Some(ReadSelect) else None

  private def canReadSelect(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "select") Some(ReadSelect) else None

  private def canReadFunction(c: Char, valueSoFar: String, history: Seq[State]) =
    if(valueSoFar.nonEmpty && valueSoFar(0) != '"' && (c == ' ' || c == '(' || operators.contains(c)) && functions.contains(valueSoFar)) Some(ReadFunction)
    else None

  private def canReadField(c: Char, valueSoFar: String, history: Seq[State]) =
    if(valueSoFar.nonEmpty &&
      !functions.contains(valueSoFar.toLowerCase) &&
      !valueSoFar.forall(_.isDigit) &&
      (c == ' ' || c == ',' || operators.contains(c))) Some(ReadField)
    else None

  private def canReadConstant(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.nonEmpty &&
       (valueSoFar.toBooleanOption.isDefined || valueSoFar.forall(_.isDigit) || valueSoFar(0) == '"') &&
       (c == ' ' || c == ',' || operators.contains(c))) Some(ReadConstant)
    else None

  private def canReadOperator(c: Char, valueSoFar: String, history: Seq[State]) =
    if (operators.contains(c)) Some(ReadOperator) else None

  private def canReadFrom(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "from") Some(ReadFrom) else None

  private def canReadWhere(c: Char, valueSoFar: String, history: Seq[State]) =
    if (valueSoFar.toLowerCase == "where") Some(ReadWhere) else None
}
