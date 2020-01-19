package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.TokenKind.Other
import enumeratum._

sealed abstract class Token(val name: String = "", val kind: TokenKind = Other) extends EnumEntry

object Token extends Enum[Token] {
  val values: IndexedSeq[Token] = findValues
  val functions: Set[Token] = Set(Sum, Avg, Median, Count, Max, Min)

  case object Default extends Token

  case object Sum extends Token("sum", TokenKind.Function)
  case object Avg extends Token("avg", TokenKind.Function)
  case object Median extends Token("median", TokenKind.Function)
  case object Count extends Token("count", TokenKind.Function)
  case object Max extends Token("max", TokenKind.Function)
  case object Min extends Token("min", TokenKind.Function)

  case object From extends Token("", TokenKind.Keyword)

  case object Insert extends Token("", TokenKind.Keyword)
  case object Select extends Token("", TokenKind.Keyword)
  case object Update extends Token("", TokenKind.Keyword)
  case object Delete extends Token("", TokenKind.Keyword)

  case object Where extends Token("", TokenKind.Keyword)
  case object And extends Token("", TokenKind.Keyword)
  case object Or extends Token("", TokenKind.Keyword)

  case class Json(value: JValue) extends Token("", TokenKind.Other)
  case class Bracket(value: String) extends Token("", TokenKind.Other) {
    val isOpening: Boolean = value == "("
  }
  case class Field(value: String) extends Token("", TokenKind.Other)
  case class Constant(value: String) extends Token("", TokenKind.Other)
  case class Operator(value: String) extends Token("", TokenKind.Other)
}
