package com.mmalek.jsonSql.jsonParsing.extractors

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JBool, JDouble, JInt, JNothing, JNull, JString, JValue}

class ScalarValueExtractor(builder: StringBuilder) {
  def next(char: Char): ScalarValueExtractor = {
    builder.append(char)

    this
  }

  def isScalarValue: Boolean = builder.nonEmpty && inScalarValue

  def flush: (JValue, ScalarValueExtractor) = {
    val value = builder.toString
    val colonIdx = value.indexOf(':')
    val closingCharIdx =
      (value.lastIndexOf(',') match {
        case -1 => value.lastIndexOf('}')
        case x => x
      }) match {
        case -1 => value.lastIndexOf(']')
        case x => x
      }

    if (colonIdx > -1 && closingCharIdx > -1) {
      val initiallyCleanedValue = value.substring(colonIdx + 1, closingCharIdx).trim.stripLineEnd
      val finalValue = getJsonValue(
        if (initiallyCleanedValue(0) == '"') initiallyCleanedValue.substring(1, initiallyCleanedValue.length - 1)
        else initiallyCleanedValue)

      (finalValue, new ScalarValueExtractor(new StringBuilder))
    } else (JNothing(0), new ScalarValueExtractor(new StringBuilder))
  }

  private def getJsonValue(scalar: String) =
    if (scalar.forall(_.isDigit)) JInt(BigInt(scalar))
    else scalar
      .toDoubleOption
      .map(d => JDouble(d))
      .getOrElse(scalar
        .toBooleanOption
        .map(b => JBool(b))
        .getOrElse(if (scalar == "null") JNull(0) else JString(scalar)))

  private def inScalarValue = {
    val noWhitespaces = builder.toString.replaceAll(" +", "")

    if (noWhitespaces.length == 0) false
    else {
      val firstChar = noWhitespaces.charAt(0)

      firstChar == ':' && (noWhitespaces.last == ',' || noWhitespaces.last == '}' || noWhitespaces.last == ']')
    }
  }
}
