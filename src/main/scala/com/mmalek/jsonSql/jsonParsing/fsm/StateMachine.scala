package com.mmalek.jsonSql.jsonParsing.fsm

import com.mmalek.jsonSql.jsonParsing.fsm.State._

import scala.collection.immutable.HashMap

class StateMachine(val state: State) {
  private val transitions: HashMap[State, Seq[(Char, StringBuilder, Seq[State]) => Option[State]]] = HashMap(
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
      canReadObjectKeyAfterSomeEnd,
      canReadScalarAfterSomeEnd,
      canReadArrayEndAfterSomeEnd,
      canReadObjectEndAfterSomeEnd))

  def next(c: Char, sb: StringBuilder, history: Seq[State]): Option[StateMachine] =
    transitions(state).flatMap(f => f(c, sb, history)) match {
      case x :: Nil => Some(new StateMachine(x))
      case _ => None
    }

  private def canReadObject(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '{') Some(ReadObject) else None

  private def canReadArray(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '[') Some(ReadArray) else None

  private def canReadObjectKeyAfterObjectOpen(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '"') Some(ReadObjectKey) else None

  private def canReadObjectEndAfterObjectOpen(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '}') Some(ReadObjectEnd) else None

  private def canReadArrayEndAfterArrayOpen(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == ']') Some(ReadArrayEnd) else None

  private def canReadScalarAfterArrayOpen(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c != '{' && c != '[' && c != ']') Some(ReadScalar) else None

  private def canReadScalarAfterObjectKey(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (isKeyReadFully(sb) && isLastRememberedCharColon(sb) && c != '{' && c != '[' && c != ' ') Some(ReadScalar) else None

  private def canReadObjectAfterObjectKey(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (isKeyReadFully(sb)) canReadObject(c, sb, history) else None

  private def canReadArrayAfterObjectKey(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (isKeyReadFully(sb)) canReadArray(c, sb, history) else None

  private def canReadObjectKeyAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == ',') history match {
      case _ :: x :: _ if x == ReadObjectKey & isScalarReadFully(c, sb) => Some(ReadObjectKey)
      case _ => None
    }
    else None

  private def canReadObjectAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '{') history match {
      case _ :: xs if isIn(ReadArray, ReadArrayEnd, xs) => Some(ReadObject)
      case _ => None
    }
    else None

  private def canReadObjectEndAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '}') (sb
      .toString
      .replaceAll(" +", "")
      .replaceAll("\"", ""), history) match {
      case (i, _ :: x :: _)
        if (x == ReadObjectKey & i.startsWith("\"") & i.endsWith("\"")) |
          (x == ReadObjectKey & !i.startsWith("\"")) => Some(ReadObjectEnd)
      case _ => None
    }
    else None

  private def canReadArrayAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '[') history match {
      case _ :: xs if isIn(ReadArray, ReadArrayEnd, xs) => Some(ReadArray)
      case _ => None
    }
    else None

  private def canReadArrayEndAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == ']') history match {
      case _ :: xs if isIn(ReadArray, ReadArrayEnd, xs) => Some(ReadArrayEnd)
      case _ => None
    }
    else None

  private def canReadScalarAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == ',') history match {
      case _ :: xs if isScalarReadFullyInArray(c, sb, xs) => Some(ReadScalar)
      case _ => None
    }
    else None

  private def canReadObjectAfterObjectEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '{' && isIn(ReadArray, ReadArrayEnd, history)) Some(ReadObject) else None

  private def canReadObjectKeyAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '"' && isIn(ReadObject, ReadObjectEnd, history)) Some(ReadObjectKey) else None

  private def canReadObjectEndAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '}' && isIn(ReadObject, ReadObjectEnd, history)) Some(ReadObjectEnd) else None

  private def canReadArrayEndAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == ']' && isIn(ReadArray, ReadArrayEnd, history)) Some(ReadArrayEnd) else None

  private def canReadScalarAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c != '{' && c != '[' && c != ']' && isIn(ReadArray, ReadArrayEnd, history)) Some(ReadScalar) else None

  private def isIn(openState: State, closeState: State, history: Seq[State]) =
    history.foldLeft((false, 0))((aggregate, state) => {
      val (foundArray, openBracketsCnt) = aggregate

      if (!foundArray && state == openState && openBracketsCnt == 0) (true, 1)
      else if (!foundArray && state == openState && openBracketsCnt > 0) (false, openBracketsCnt - 1)
      else if (!foundArray && state == closeState) (false, openBracketsCnt + 1)
      else aggregate
    })._1

  private def isKeyReadFully(sb: StringBuilder) =
    sb
      .toString
      .replaceAll(" +", "")
      .replaceAll("\\\\\"", "")
      .count(c => c == '"') % 2 == 0

  private def isScalarReadFullyInArray(c: Char, sb: StringBuilder, history: Seq[State]) = {
    if (!isIn(ReadArray, ReadArrayEnd, history)) false
    else {
      isScalarReadFully(c, sb)
    }
  }

  private def isScalarReadFully(c: Char, sb: StringBuilder) = {
    val value = sb
      .toString
      .replaceAll(" +", "")
      .replaceAll("\\\\\"", "")

    if (value(0) == '"') value.last == '"'
    else c == ','
  }

  private def isLastRememberedCharColon(sb: StringBuilder) = {
    sb.toString.replaceAll(" +", "").replaceAll(": +", "").last == ':'
  }
}
