package com.mmalek.jsonSql.jsonParsing.extractors

case class PropertyNameExtractor(builder: StringBuilder) {
  def next(char: Char): PropertyNameExtractor = {
    builder.append(char)

    this
  }

  def isProperty: Boolean = builder.nonEmpty && inPropertyName

  def flush: (String, PropertyNameExtractor) = {
    val value = builder.toString
    val firstQuoteIdx = value.indexOf('"')
    val secondQuoteIdx = value.indexOf('"', firstQuoteIdx + 1)
    val propertyName = value.substring(firstQuoteIdx, secondQuoteIdx)

    (propertyName, PropertyNameExtractor(new StringBuilder))
  }

  private def inPropertyName = {
    val noWhitespaces = builder.toString.replaceAll(" +", "")
    val firstChar = noWhitespaces.charAt(0)

    firstChar == ',' || firstChar == '{'
  }
}
