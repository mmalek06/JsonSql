package com.mmalek.jsonSql.jsonParsing.extractors

class ScalarValueExtractor(builder: StringBuilder) {
  def next(char: Char): ScalarValueExtractor = {
    builder.append(char)

    this
  }

  def isScalarValue: Boolean = builder.nonEmpty && inScalarValue

  def flush: (String, ScalarValueExtractor) = {
    val value = builder.toString
    val firstQuoteIdx = value.indexOf('"')
    val secondQuoteIdx = value.indexOf('"', firstQuoteIdx + 1)
    val propertyName = value.substring(firstQuoteIdx, secondQuoteIdx)

    (propertyName, new ScalarValueExtractor(new StringBuilder))
  }

  private def inScalarValue = {
    val noWhitespaces = builder.toString.replaceAll(" +", "")
    val firstChar = noWhitespaces.charAt(0)

    firstChar == ':'
  }
}
