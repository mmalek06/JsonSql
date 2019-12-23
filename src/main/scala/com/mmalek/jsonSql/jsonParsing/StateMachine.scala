package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.fsm.State
import com.mmalek.jsonSql.jsonParsing.fsm.State._

import scala.collection.immutable.HashMap

class StateMachine {
  private val transitions = HashMap(
    Initial -> Seq(
      canReadObject _,
      canReadArray _),
    ReadObject -> Seq(
      canReadObjectKeyAfterObjectOpen _,
      canReadObjectEndAfterObjectOpen _),
    ReadArray -> Seq(
      canReadObject _,
      canReadScalarAfterArrayOpen _,
      canReadArray _,
      canReadArrayEndAfterArrayOpen _),
    ReadObjectKey -> Seq(
      canReadScalarAfterObjectKey _,
      canReadObject _,
      canReadArray _),
    ReadScalar -> Seq(
      canReadObjectKeyAfterScalar _,
      canReadObjectAfterScalar _,
      canReadObjectEndAfterScalar _,
      canReadArrayAfterScalar _,
      canReadArrayEndAfterScalar _,
      canReadScalarAfterScalar _),
    ReadObjectEnd -> Seq(
      canReadObjectAfterObjectEnd _,
      canReadObjectKeyAfterSomeEnd _,
      canReadObjectEndAfterSomeEnd _,
      canReadArrayEndAfterSomeEnd _,
      canReadScalarAfterSomeEnd _),
    ReadArrayEnd -> Seq(
      canReadObjectKeyAfterSomeEnd _,
      canReadScalarAfterSomeEnd _,
      canReadArrayEndAfterSomeEnd _,
      canReadObjectEndAfterSomeEnd _))

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
    if (c != '{' && c != '[') Some(ReadScalar) else None

  private def canReadObjectKeyAfterScalar(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '"') history match {
      case _ :: x :: _
        if x == ReadObjectKey &
          sb.toString.replaceAll(" +", "").last == ',' => Some(ReadObjectKey)
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
    if (c == '{' && isIn(ReadArray, ReadArrayEnd, history.tail)) Some(ReadObject) else None

  private def canReadObjectKeyAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '"' && isIn(ReadObject, ReadObjectEnd, history.tail)) Some(ReadObjectKey) else None

  private def canReadObjectEndAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == '}' && isIn(ReadObject, ReadObjectEnd, history.tail)) Some(ReadObjectEnd) else None

  private def canReadArrayEndAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c == ']' && isIn(ReadArray, ReadArrayEnd, history.tail)) Some(ReadArrayEnd) else None

  private def canReadScalarAfterSomeEnd(c: Char, sb: StringBuilder, history: Seq[State]) =
    if (c != '{' && c != '[' && c != ']' && isIn(ReadArray, ReadArrayEnd, history.tail)) Some(ReadScalar) else None

  private def isIn(openState: State, closeState: State, history: Seq[State]) =
    history.foldLeft((false, 0))((aggregate, state) => {
      val (foundArray, openBracketsCnt) = aggregate

      if (!foundArray && state == openState && openBracketsCnt == 0) (true, 1)
      else if (!foundArray && state == openState && openBracketsCnt > 0) (false, openBracketsCnt - 1)
      else if (!foundArray && state == closeState) (false, openBracketsCnt + 1)
      else aggregate
    })._1

  private def isScalarReadFullyInArray(c: Char, sb: StringBuilder, history: Seq[State]) = {
    if (!isIn(ReadArray, ReadArrayEnd, history)) false
    else {
      val value = sb
        .toString
        .replaceAll(" +", "")
        .replaceAll("\"", "")

      if (value(0) == '"') value.last == '"'
      else c == ','
    }
  }
}
