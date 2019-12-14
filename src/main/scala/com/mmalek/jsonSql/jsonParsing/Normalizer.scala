package com.mmalek.jsonSql.jsonParsing

object Normalizer {
  private val newLineChar = '\n'
  private val carriageReturnChar = '\r'

  def normalize(json: String): String =
    json.foldLeft((new StringBuilder, 0.toInt))((aggregate, char) => {
      val (builder, oldOpenQuotes) = aggregate
      val openQuotes = getOpenQuotesNum(char, oldOpenQuotes)

      if (openQuotes != 0) (builder.append(char), openQuotes)
      else if (char != newLineChar && char != carriageReturnChar) appendConditionally(char, builder, openQuotes)
      else (builder, openQuotes)
    })._1.toString

  private def getOpenQuotesNum(char: Char, oldOpenQuotes: Int) =
    if (char == '"' && oldOpenQuotes == 1) 0
    else if (char == '"') 1
    else oldOpenQuotes

  private def appendConditionally(char: Char, builder: StringBuilder, openQuotes: Int) =
    if (builder.nonEmpty && builder.last == ' ' && char == ' ') (builder, openQuotes)
    else (builder.append(char), openQuotes)
}
