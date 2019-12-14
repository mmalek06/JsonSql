package com.mmalek.jsonSql.sqlParsing.tokens

import scala.collection.immutable.HashSet

object Tokenizer {
  private val stopChars = HashSet[Char](' ', ',', '(', ')')
  private val mappedTokens = mapTokens

  def tokenize(input: String): Seq[Token] =
    cleanInput _ andThen
    makeTokens apply input

  private def mapTokens =
    Token.values
      .map(t => t.toString.toLowerCase -> t)
      .filterNot(pair => pair._1 == "Any")
      .toMap

  private def cleanInput(input: String) =
    input
      .replaceAll(" +", " ")
      .replaceAll(", ", ",")
      .replaceAll(" ,", ",")

  private def makeTokens(input: String): Seq[Token] = {
    val (tokens, builder) = input
      .foldLeft((List.empty[Token], new StringBuilder))((aggregate, char) => {
        val isStopChar = stopChars.contains(char)
        val sb = aggregate._2

        if (isStopChar && sb.isEmpty) aggregate
        else if (isStopChar) getNewAggregate(aggregate)
        else (aggregate._1, aggregate._2.append(char))
      })
    val aggregate = getNewAggregate(tokens, builder)

    aggregate._1
  }

  private def getNewAggregate(oldAggregate: (List[Token], StringBuilder)) = {
    val value = oldAggregate._2.toString.trim
    val token = mappedTokens.getOrElse(value.toLowerCase, Token.Any(value))
    val list = oldAggregate._1 :+ token
    val sb = new StringBuilder

    (list, sb)
  }
}
