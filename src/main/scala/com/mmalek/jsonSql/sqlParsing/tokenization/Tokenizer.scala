package com.mmalek.jsonSql.sqlParsing.tokenization

import com.mmalek.jsonSql.sqlParsing.extraction.JsonExtractor
import com.mmalek.jsonSql.sqlParsing.tokenization.Token._

import scala.collection.immutable.HashSet

object Tokenizer {
  private val stopChars = HashSet[Char](' ', ',', '(', ')')
  private val mappedTokens = mapTokens

  def tokenize(input: String): Seq[Token] = {
    val seed = TokenizingTuple(List.empty[Token], new StringBuilder, JsonExtractor(new StringBuilder, 0, 0))
    val tokens = createTokens(input, seed)
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
      .foldLeft(seed)((aggregate, char) => {
        if (aggregate.tokens.nonEmpty && aggregate.tokens.last == From && !aggregate.jsonExtractor.isReadFully) TokenizingTuple(
          aggregate.tokens,
          aggregate.expressionBuilder,
          aggregate.jsonExtractor.next(char))
        else if (aggregate.jsonExtractor.isReadFully) makeJsonToken(aggregate, char)
        else makeStandardTokens(aggregate, char)
      })

  private def makeJsonToken(aggregate: TokenizingTuple, char: Char) = {
    val (json, extractor) = aggregate.jsonExtractor.flush
    val newTokens = aggregate.tokens :+ Json(json)

    TokenizingTuple(
      newTokens,
      aggregate.expressionBuilder.append(char),
      extractor)
  }

  private def makeStandardTokens(aggregate: TokenizingTuple, char: Char) = {
    val isStopChar = stopChars.contains(char)
    val sb = aggregate.expressionBuilder

    if (isStopChar && sb.isEmpty) aggregate
    else if (isStopChar) getNewAggregate(aggregate)
    else TokenizingTuple(
      aggregate.tokens,
      aggregate.expressionBuilder.append(char),
      aggregate.jsonExtractor)
  }

  private def getNewAggregate(oldAggregate: TokenizingTuple) = {
    val value = oldAggregate.expressionBuilder.toString.trim.toLowerCase
    val token = mappedTokens.getOrElse(value, Token.Any(value))
    val list = oldAggregate.tokens :+ token

    TokenizingTuple(list, new StringBuilder, oldAggregate.jsonExtractor)
  }

  private def filterOutEmpty(tokens: Seq[Token]) =
    tokens.filterNot {
      case Any("") => true
      case _ => false
    }

  private case class TokenizingTuple(tokens: Seq[Token],
                                     expressionBuilder: StringBuilder,
                                     jsonExtractor: JsonExtractor)
}
