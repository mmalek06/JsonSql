package com.mmalek.jsonSql.execution.selectStrategies.folding

import com.mmalek.jsonSql.execution.TokensInfo
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.{AddOperator, AvgFunction}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Bracket, Default, Field, Operator}

object FoldingStrategy {
  private val runnables = Seq(
    new AddOperator,
    new AvgFunction)

  def apply(tokens: Seq[Token], json: JValue, tokensInfo: TokensInfo): Map[String, Seq[Option[JValue]]] = {
    val partitions = partition(tokens)
    val groupedPartitions = groupPartitions(partitions)
    val rnpPartitions = groupedPartitions.arithmetic.map(p => Infix2RpnConverter.convert(p))
    val runnablePartitions = groupedPartitions.copy(arithmetic = rnpPartitions)
    val arithmeticResults = runnablePartitions.arithmetic.map(runOps(_))
    val otherResults = runnablePartitions.other.map(runOps(_))

    ???
  }

  private def partition(tokens: Seq[Token]) = {
    val seed = PartitionsTuple(List.empty[Seq[Token]], Default)
    val nextAggregate = (aggregate: PartitionsTuple, token: Token) =>
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

  private def groupPartitions(partitions: Seq[Seq[Token]]) =
    partitions
      .foldLeft(PartitionGroups(Nil, Nil))((aggregate, p) =>
        if (hasOperator(p)) aggregate.copy(arithmetic = aggregate.arithmetic :+ p)
        else aggregate.copy(other = aggregate.other :+ p))

  private def hasOperator(partition: Seq[Token]) =
    partition.exists {
      case _: Operator => true
      case _ => false
    }

  private def runOps(tokens: Seq[Token]) = ???
//    tokens.foldLeft(Seq.empty[RunnableArgument]) {
//      case
//    }

  private def isFunction(x: Token) =
    Token.functions.exists(t => t.name == x.name)

  private case class PartitionsTuple(partitionedTokens: Seq[Seq[Token]],
                                     previousToken: Token)

  private case class PartitionGroups(arithmetic: Seq[Seq[Token]],
                                     other: Seq[Seq[Token]])
}
