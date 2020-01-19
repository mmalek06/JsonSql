package com.mmalek.jsonSql.execution.selectStrategies.folding

import com.mmalek.jsonSql.execution.TokensInfo
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Bracket, Default, Field, Operator}

import scala.collection.mutable

object FoldingStrategy {
  def apply(tokens: Seq[Token], json: JValue, tokensInfo: TokensInfo): Map[String, Seq[Option[JValue]]] = {
    val partitions = partition(tokens)
    val arithmeticPartitions = getArithmeticPartitions(partitions)
    val rnpPartitions = arithmeticPartitions.map(p => Infix2RpnConverter.convert(p))

    ???
  }

  private def partition(tokens: Seq[Token]) = {
    val seed = PartitioningTuple(List.empty[Seq[Token]], Default)
    val nextAggregate = (aggregate: PartitioningTuple, token: Token) =>
      aggregate.copy(partitionedTokens = aggregate.partitionedTokens :+ Seq(token))

    tokens.foldLeft(seed)((aggregate, token) =>
      (aggregate.previousToken, token) match {
        case (b: Bracket, _: Field) if !b.isOpening => nextAggregate(aggregate, token)
        case (b: Bracket, x) if !b.isOpening & isFunction(x) => nextAggregate(aggregate, token)
        case (_: Field, _: Field) => nextAggregate(aggregate, token)
        case (_: Field, x) if isFunction(x) => nextAggregate(aggregate, token)
        case _ if aggregate.partitionedTokens.isEmpty => aggregate.copy(
          partitionedTokens = Seq(Seq(token)),
          previousToken = token)
        case _ => aggregate.copy(
          partitionedTokens = aggregate.partitionedTokens.init :+ (aggregate.partitionedTokens.last :+ token),
          previousToken = token)
      }).partitionedTokens
  }

  private def getArithmeticPartitions(partitions: Seq[Seq[Token]]) =
    partitions.filter(p => p.exists {
      case _: Operator => true
      case _ => false
    })

  private def isFunction(x: Token) =
    Token.functions.exists(t => t.name == x.name)

  private def makeRnp(tokens: Seq[Token]) = {
    val prededence = getPrecedence
    val queue = mutable.Queue[Token]()
    val s = mutable.Stack[Token]()
  }

  private def getPrecedence =
    Map[String, Int](
      "/" -> 5,
      "*" -> 5,
      "%" -> 5,
      "+" -> 4,
      "-" -> 4,
      "(" -> 0)

  private def rnp(tokens: Seq[Token]) = {

  }

  private case class PartitioningTuple(partitionedTokens: Seq[Seq[Token]],
                                       previousToken: Token)
}
