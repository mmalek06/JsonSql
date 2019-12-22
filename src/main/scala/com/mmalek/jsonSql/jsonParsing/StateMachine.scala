package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.fsm.State
import com.mmalek.jsonSql.jsonParsing.fsm.State._

import scala.collection.immutable.HashMap

class StateMachine {
  private val canReadObjectKeyAfterScalar = (_: Char, sb: StringBuilder, history: Seq[State]) =>
    history match {
      case _ :: x :: _
        if x == ReadObjectKey &
           sb.toString.replaceAll(" +", "").last == ',' => Some(ReadObjectKey)
      case _ => None
    }

  private val canReadObjectAfterScalar = (_: Char, _: StringBuilder, history: Seq[State]) =>
    history match {
      case _ :: xs if isInArray(xs) => Some(ReadObject)
      case _ => None
    }

  private val transitions = HashMap(
    Initial -> Seq(
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObject),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArray)),
    ReadObject -> Seq(
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectKey),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectEnd)),
    ReadArray -> Seq(
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObject),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadScalar),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArray),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArrayEnd)),
    ReadObjectKey -> Seq(
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadScalar),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObject),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArray)),
    ReadScalar -> Seq(
      canReadObjectKeyAfterScalar,
      canReadObjectAfterScalar,
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectEnd),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArray),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArrayEnd),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadScalar)),
    ReadObjectEnd -> Seq(
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObject),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectKey),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadScalar),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectEnd),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArrayEnd)),
    ReadArrayEnd -> Seq(
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectKey),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadScalar),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadArrayEnd),
      (c: Char, sb: StringBuilder, _: Seq[State]) => Some(ReadObjectEnd)))

  private def isInArray(history: Seq[State]) =
    history.foldLeft((false, 0))((aggregate, state) => {
      val (foundArray, openBracketsCnt) = aggregate

      if (!foundArray && state == ReadArray && openBracketsCnt == 0) (true, 1)
      else if (!foundArray && state == ReadArray && openBracketsCnt > 0) (false, openBracketsCnt - 1)
      else if (!foundArray && state == ReadArrayEnd) (false, openBracketsCnt + 1)
      else aggregate
    })._1
}
