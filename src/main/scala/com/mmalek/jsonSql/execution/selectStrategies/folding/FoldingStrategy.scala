package com.mmalek.jsonSql.execution.selectStrategies.folding

import com.mmalek.jsonSql.execution.TokensInfo
import com.mmalek.jsonSql.execution.extensions.StringOps._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.{AddOperator, AvgFunction}
import com.mmalek.jsonSql.execution.selectStrategies.MappingStrategy
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.Coproduct

object FoldingStrategy {
  private val operators = Seq(
    new AddOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(tokens: Seq[Token], json: JValue, tokensInfo: TokensInfo): Map[String, Seq[Option[JValue]]] = {
    val partitions = partition(tokens)
    val groupedPartitions = groupPartitions(partitions)
    val rnpPartitions = groupedPartitions.arithmetic.map(p => Infix2RpnConverter.convert(p))
    val runnablePartitions = groupedPartitions.copy(arithmetic = rnpPartitions)
    val arithmeticResults = runnablePartitions.arithmetic.map(runOps(_, json))
    val otherResults = runnablePartitions.other.map(MappingStrategy(_, json))

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

  private def runOps(tokens: Seq[Token], json: JValue) =
    tokens.foldLeft(Right(Seq.empty[RunnableArgument]).withLeft[String])((aggOrError, t) => (aggOrError, t) match {
      case (Right(aggregate), x: Constant) => getConstant(aggregate, x)
      case (Right(aggregate), x: Field) => Right(aggregate :+ Coproduct[RunnableArgument](x))
      case (Right(aggregate), x: Operator) => runOperator(aggregate, x)
      case (Right(aggregate), x: Function) => runFunction(aggregate, json, x)
      case _ => aggOrError
    })

  private def getConstant(aggregate: Seq[RunnableArgument], const: Constant) =
    if (const.value.isNumber) Right(aggregate :+ Coproduct[RunnableArgument](BigDecimal(const.value)))
    else Right(aggregate :+ Coproduct[RunnableArgument](const.value))

  private def runOperator(aggregate: Seq[RunnableArgument], x: Operator) =
    operators
      .find(_.canRun(x.value, aggregate))
      .flatMap(_.run(aggregate))
      .map(value => Right(aggregate :+ value))
      .getOrElse(Left(s"Couldn't run ${x.value} operator, because the input was in bad format. Aborting..."))

  private def runFunction(aggregate: Seq[RunnableArgument], json: JValue, x: Function) =
    functions
      .find(_.canRun(x.name, aggregate))
      .flatMap(_.run(aggregate, Some(json)))
      .map(value => Right(aggregate :+ value))
      .getOrElse(Left(s"Couldn't run ${x.name} function, because the input was in bad format. Aborting..."))

  private def isFunction(x: Token) =
    Token.functions.contains(x.name)

  private case class PartitionsTuple(partitionedTokens: Seq[Seq[Token]],
                                     previousToken: Token)

  private case class PartitionGroups(arithmetic: Seq[Seq[Token]],
                                     other: Seq[Seq[Token]])
}
