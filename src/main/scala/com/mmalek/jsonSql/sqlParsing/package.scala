package com.mmalek.jsonSql

import com.mmalek.jsonSql.sqlParsing.Token.{Any, From}

import scala.collection.immutable.HashSet

package object sqlParsing {
  private val stopChars = HashSet[Char](' ', ',', '(', ')')
  private val mappedTokens = mapTokens

  def tokenize(input: String): Seq[Token] = {
    val seed = TokenizingTuple(List.empty[Token], new StringBuilder)
    val cleanedInput = input.replace("##json##", "")
    val tokens = createTokens(cleanedInput, seed)
    val aggregate = getNewAggregate(tokens)
    val result = filterOutEmpty(aggregate.tokens)

    result
  }

  private def mapTokens =
    Token.values
      .map(t => t.toString.toLowerCase -> t)
      .filterNot(pair => pair._1 == Any.toString || pair._1 == From.toString)
      .toMap

  private def createTokens(input: String, seed: TokenizingTuple) =
    input
      .trim
      .foldLeft(seed)((aggregate, char) => makeStandardTokens(aggregate, char))

  private def makeStandardTokens(aggregate: TokenizingTuple, char: Char) = {
    val isStopChar = stopChars.contains(char)
    val sb = aggregate.expressionBuilder

    if (isStopChar && sb.isEmpty) aggregate
    else if (isStopChar) getNewAggregate(aggregate)
    else aggregate.copy(expressionBuilder = aggregate.expressionBuilder.append(char))
  }

  private def getNewAggregate(oldAggregate: TokenizingTuple) = {
    val value = oldAggregate.expressionBuilder.toString.trim.toLowerCase
    val token = mappedTokens.getOrElse(value, Token.Any(value))
    val list = oldAggregate.tokens :+ token

    TokenizingTuple(list, new StringBuilder)
  }

  private def filterOutEmpty(tokens: Seq[Token]) =
    tokens.filterNot {
      case Any("") => true
      case _ => false
    }

  private case class TokenizingTuple(tokens: Seq[Token],
                                     expressionBuilder: StringBuilder)
}
