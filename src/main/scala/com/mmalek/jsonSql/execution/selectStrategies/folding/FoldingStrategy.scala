package com.mmalek.jsonSql.execution.selectStrategies.folding

import com.mmalek.jsonSql.execution.TokensInfo
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.{AddOperator, AvgFunction}
import com.mmalek.jsonSql.execution.selectStrategies.MappingStrategy
import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.{Coproduct, Inl, Inr}

object FoldingStrategy {
  private val operators = Seq(
    new AddOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(tokens: Seq[Token], json: JValue, tokensInfo: TokensInfo): Either[String, Map[String, Seq[Option[JValue]]]] = {
    val partitions = partition(tokens)
    val results = partitions
      .foldLeft(Right(Seq.empty[(String, Seq[Option[JValue]])]).withLeft[String])((aggregate, partition) =>
        aggregate match {
          case Right(agg) => calculateResults(json, partition, agg)
          case _ => aggregate
        })
      .map(_.toMap)

    results
  }

  private def calculateResults(json: JValue, partition: Seq[Token], agg: Seq[(String, Seq[Option[JValue]])]) =
    partition match {
      case p if isAlias(p) =>
        (p.collectFirst { case FieldAlias(value) => value })
          .map((key: String) => Right(Seq(key -> agg.last._2)))
          .getOrElse(Left("Alias expected, but not given. Parsing aborted..."))
          .map(vals => agg.init :+ vals.head)
      case p if hasOperator(p) => runOps(Infix2RpnConverter.convert(p), json).map(agg :+ _.head)
      case p => MappingStrategy(p, json).map(agg :+ _.head)
    }

  private def partition(tokens: Seq[Token]) = {
    val seed = PartitionsTuple(List.empty[Seq[Token]], Default)
    val nextAggregate = (aggregate: PartitionsTuple, token: Token) =>
      aggregate.copy(partitionedTokens = aggregate.partitionedTokens :+ Seq(token))

    tokens.foldLeft(seed)((aggregate, token) =>
      (aggregate.previousToken, token) match {
        case (b: Bracket, _: Field) if !b.isOpening => nextAggregate(aggregate, token)
        case (b: Bracket, Function(_)) if !b.isOpening => nextAggregate(aggregate, token)
        case (_: FieldAlias, _: Field) => nextAggregate(aggregate, token)
        case (_: Field, _: Field) => nextAggregate(aggregate, token)
        case (_: Field, Function(_)) => nextAggregate(aggregate, token)
        case (_, As) => nextAggregate(aggregate, token)
        case _ if aggregate.partitionedTokens.isEmpty => aggregate.copy(
          partitionedTokens = Seq(Seq(token)),
          previousToken = token)
        case _ => aggregate.copy(
          partitionedTokens = aggregate.partitionedTokens.init :+ (aggregate.partitionedTokens.last :+ token),
          previousToken = token)
      }).partitionedTokens
  }

  private def hasOperator(partition: Seq[Token]) =
    partition.exists {
      case _: Operator => true
      case _: Function => true
      case _ => false
    }

  private def isAlias(partition: Seq[Token]) =
    partition.exists {
      case _: FieldAlias => true
      case As => true
      case _ => false
    }

  private def runOps(tokens: Seq[Token], json: JValue): Either[String, Map[String, Seq[Option[JValue]]]] =
    tokens.foldLeft(Right(Seq.empty[RunnableArgument]).withLeft[String])((aggOrError, t) => (aggOrError, t) match {
      case (Right(aggregate), x: Constant) => getConstant(aggregate, x)
      case (Right(aggregate), x: Field) => Right(aggregate :+ Coproduct[RunnableArgument](x))
      case (Right(aggregate), x: Operator) => runOperator(aggregate, x)
      case (Right(aggregate), x: Function) => runFunction(aggregate, json, x)
      case _ => aggOrError
    }) match {
      case Right(Seq(Inl(Field(name)), Inr(Inl(value)))) => Right(Map(name -> Seq(Some(value.toString.asJValue))))
      case Right(_) => Left("Something went wrong...")
      case Left(x) => Left(x)
    }

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

  private case class PartitionsTuple(partitionedTokens: Seq[Seq[Token]],
                                     previousToken: Token)

  private case class PartitionGroups(arithmetic: Seq[Seq[Token]],
                                     other: Seq[Seq[Token]])
}
