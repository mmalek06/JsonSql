package com.mmalek.jsonSql.jsonParsing.extractors

class ScalarValueExtractor(builder: StringBuilder) {
  def next(char: Char): ScalarValueExtractor = {
    builder.append(char)

    this
  }

  def isScalarValue: Boolean = builder.nonEmpty && inScalarValue

  def flush: (String, ScalarValueExtractor) = {
    val value = builder.toString
    val colonIdx = value.indexOf(':')
    val commaIdx = value.lastIndexOf(',')

    if (colonIdx > -1 && commaIdx > -1) {
      val initiallyCleanedValue = value.substring(colonIdx + 1, commaIdx).trim.stripLineEnd
      val finalValue =
        if (initiallyCleanedValue(0) == '"') initiallyCleanedValue.substring(1, initiallyCleanedValue.length - 1)
        else initiallyCleanedValue

      (finalValue, new ScalarValueExtractor(new StringBuilder))
    } else ("", new ScalarValueExtractor(new StringBuilder))
  }

  private def inScalarValue = {
    val noWhitespaces = builder.toString.replaceAll(" +", "")

    if (noWhitespaces.length == 0) false
    else {
      val firstChar = noWhitespaces.charAt(0)

      firstChar == ':' && noWhitespaces.last == ','
    }
  }
}
