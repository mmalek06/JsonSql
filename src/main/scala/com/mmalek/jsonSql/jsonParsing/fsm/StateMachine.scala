package com.mmalek.jsonSql.jsonParsing.fsm

import com.mmalek.jsonSql.jsonParsing.fsm.State._

import scala.collection.immutable.HashMap

class StateMachine(val state: State) {
  private val transitions: HashMap[State, Seq[(Char, String, Seq[State]) => Option[State]]] = HashMap(
    State.Initial -> Seq(
      canReadObject,
      canReadArray),
    State.ReadObject -> Seq(
      canReadObjectKeyAfterObjectOpen,
      canReadObjectEndAfterObjectOpen),
    State.ReadArray -> Seq(
      canReadObject,
      canReadScalarAfterArrayOpen,
      canReadArray,
      canReadArrayEndAfterArrayOpen),
    State.ReadObjectKey -> Seq(
      canReadScalarAfterObjectKey,
      canReadObjectAfterObjectKey,
      canReadArrayAfterObjectKey),
    State.ReadScalar -> Seq(
      canReadObjectKeyAfterScalar,
      canReadObjectAfterScalar,
      canReadObjectEndAfterScalar,
      canReadArrayAfterScalar,
      canReadArrayEndAfterScalar,
      canReadScalarAfterScalar),
    State.ReadObjectEnd -> Seq(
      canReadObjectAfterObjectEnd,
      canReadObjectKeyAfterSomeEnd,
      canReadObjectEndAfterSomeEnd,
      canReadArrayEndAfterSomeEnd,
      canReadScalarAfterSomeEnd),
    State.ReadArrayEnd -> Seq(
      canReadObject,
      canReadArray,
      canReadObjectKeyAfterSomeEnd,
      canReadScalarAfterSomeEnd,
      canReadArrayEndAfterSomeEnd,
      canReadObjectEndAfterSomeEnd
    ))
  // no double quotes here, as " is a marker for new scalar
  private val specialChars = (32 to 47).toList ++ (58 to 64).toList ++ (91 to 96).toList ++ (123 to 126).toList

  def next(c: Char, sb: StringBuilder, history: Seq[State]): Option[StateMachine] = {
    val transition = transitions(state).foldLeft(Option.empty[State])((aggregate, f) => aggregate match {
      case None => f(c, sb.toString, history)
      case x => x
    })

    transition match {
      case Some(value) => Some(new StateMachine(value))
      case _ => None
    }
  }

  private def canReadObject(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '{') Some(ReadObject) else None

  private def canReadArray(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '[') Some(ReadArray) else None

  private def canReadObjectKeyAfterObjectOpen(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '"') Some(ReadObjectKey) else None

  private def canReadObjectEndAfterObjectOpen(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '}') Some(ReadObjectEnd) else None

  private def canReadArrayEndAfterArrayOpen(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == ']') Some(ReadArrayEnd) else None

  private def canReadScalarAfterArrayOpen(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c != '{' && c != '[' && c != ']' && c != ' ') Some(ReadScalar) else None

  private def canReadScalarAfterObjectKey(c: Char, valueSoFar: String, history: Seq[State]) =
    if (isKeyReadFully(valueSoFar) && isLastRememberedCharColon(valueSoFar) && c != '{' && c != '[' && c != ' ') Some(ReadScalar) else None

  private def canReadObjectAfterObjectKey(c: Char, valueSoFar: String, history: Seq[State]) =
    if (isKeyReadFully(valueSoFar)) canReadObject(c, valueSoFar, history) else None

  private def canReadArrayAfterObjectKey(c: Char, valueSoFar: String, history: Seq[State]) =
    if (isKeyReadFully(valueSoFar)) canReadArray(c, valueSoFar, history) else None

  private def canReadObjectKeyAfterScalar(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == ',') history match {
      case _ :: x :: _ if x == ReadObjectKey & isScalarReadFully(c, valueSoFar) => Some(ReadObjectKey)
      case _ => None
    }
    else None

  private def canReadObjectAfterScalar(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '{') history match {
      case _ :: xs if isIn(ReadArray, ReadArrayEnd, xs) => Some(ReadObject)
      case _ => None
    }
    else None

  private def canReadObjectEndAfterScalar(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '}') (valueSoFar
      .replaceAll(" +", "")
      .replaceAll("\"", ""), history) match {
      case (i, _ :: x :: _)
        if (x == ReadObjectKey & i.startsWith("\"") & i.endsWith("\"")) |
          (x == ReadObjectKey & !i.startsWith("\"")) => Some(ReadObjectEnd)
      case _ => None
    }
    else None

  private def canReadArrayAfterScalar(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '[') history match {
      case _ :: xs if isIn(ReadArray, ReadArrayEnd, xs) => Some(ReadArray)
      case _ => None
    }
    else None

  private def canReadArrayEndAfterScalar(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == ']') history match {
      case _ :: xs if isIn(ReadArray, ReadArrayEnd, xs) => Some(ReadArrayEnd)
      case _ => None
    }
    else None

  private def canReadScalarAfterScalar(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == ',') history match {
      case _ :: xs if isScalarReadFullyInArray(c, valueSoFar, xs) => Some(ReadScalar)
      case _ => None
    }
    else None

  private def canReadObjectAfterObjectEnd(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '{' && isIn(ReadArray, ReadArrayEnd, history)) Some(ReadObject) else None

  private def canReadObjectKeyAfterSomeEnd(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '"' && isIn(ReadObject, ReadObjectEnd, history)) Some(ReadObjectKey) else None

  private def canReadObjectEndAfterSomeEnd(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == '}' && isIn(ReadObject, ReadObjectEnd, history)) Some(ReadObjectEnd) else None

  private def canReadArrayEndAfterSomeEnd(c: Char, valueSoFar: String, history: Seq[State]) =
    if (c == ']' && isIn(ReadArray, ReadArrayEnd, history)) Some(ReadArrayEnd) else None

  private def canReadScalarAfterSomeEnd(c: Char, valueSoFar: String, history: Seq[State]) =
    if (!specialChars.contains(c) && isIn(ReadArray, ReadArrayEnd, history) && !isIn(ReadObject, ReadObjectEnd, history)) Some(ReadScalar) else None

  private def isIn(openState: State, closeState: State, history: Seq[State]) =
    history.foldLeft((false, 0))((aggregate, state) => {
      val (foundOpener, openBracketsCnt) = aggregate

      if (!foundOpener && state == openState && openBracketsCnt == 0) (true, 1)
      else if (!foundOpener && state == openState && openBracketsCnt > 0) (false, openBracketsCnt - 1)
      else if (!foundOpener && state == closeState) (false, openBracketsCnt + 1)
      else aggregate
    })._1

  private def isKeyReadFully(valueSoFar: String) =
    valueSoFar
      .replaceAll(" +", "")
      .replaceAll("\\\\\"", "")
      .count(c => c == '"') % 2 == 0

  private def isScalarReadFullyInArray(c: Char, valueSoFar: String, history: Seq[State]) = {
    if (!isIn(ReadArray, ReadArrayEnd, history)) false
    else {
      isScalarReadFully(c, valueSoFar)
    }
  }

  private def isScalarReadFully(c: Char, valueSoFar: String) = {
    val value = valueSoFar
      .replaceAll(" +", "")
      .replaceAll("\\\\\"", "")

    if (value(0) == '"') value.last == '"'
    else c == ','
  }

  private def isLastRememberedCharColon(valueSoFar: String) = {
    valueSoFar.replaceAll(" +", "").replaceAll(": +", "").last == ':'
  }
}
