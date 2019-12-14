package com.mmalek.jsonSql.sqlParsing.tokenization

case class JsonExtractor(builder: StringBuilder,
                         openObjects: Int,
                         openArrays: Int) {
  def next(char: Char): JsonExtractor = {
    builder.append(char)

    if (char == '{') updateOpenObjects(builder, 1)
    else if (char == '}') updateOpenObjects(builder, -1)
    else if (char == '[') updateOpenArrays(builder, 1)
    else if (char == ']') updateOpenArrays(builder, -1)
    else JsonExtractor(builder, openObjects, openArrays)
  }

  def updateOpenObjects(builder: StringBuilder, upOrDown: Int) =
    JsonExtractor(builder, openObjects + upOrDown, openArrays)

  def updateOpenArrays(builder: StringBuilder, upOrDown: Int) =
    JsonExtractor(builder, openObjects, openArrays + upOrDown)

  def isReadFully: Boolean = openObjects == 0 && openArrays == 0 && builder.nonEmpty

  def flush: (String, JsonExtractor) = (builder.toString, JsonExtractor(new StringBuilder, 0, 0))
}
