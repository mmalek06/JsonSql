package com.mmalek.jsonSql.execution.selection

import com.mmalek.jsonSql.execution.rpn.Infix2RpnArithmeticConverter
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.functions.AvgFunction
import com.mmalek.jsonSql.execution.runnables.operators.AddOperator
import com.mmalek.jsonSql.execution.{getConstant, runFunction, runOperator}
import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{As, Bracket, Constant, Default, Field, FieldAlias, Function, Operator}
import shapeless.{Coproduct, Inl, Inr}

object FoldingStrategy {
  private val operators = Seq(
    new AddOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(tokens: Seq[Token], json: JValue): Either[String, Map[String, Seq[Option[JValue]]]] = {
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
        p.collectFirst { case FieldAlias(value) => value }
          .map((key: String) => Right(Seq(key -> agg.last._2)))
          .getOrElse(Left("Expected alias, but none was found. Parsing aborted..."))
          .map(agg.init :+ _.head)
      case p if hasOperator(p) => runOps(Infix2RpnArithmeticConverter.convert(p), json).map(agg :+ _.head)
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
      case (Right(aggregate), x: Constant) => Right(getConstant(aggregate, x))
      case (Right(aggregate), x: Field) => Right(aggregate :+ Coproduct[RunnableArgument](x))
      case (Right(aggregate), x: Operator) => runOperator(operators, aggregate, x)
      case (Right(aggregate), x: Function) => runFunction(functions, aggregate, json, x)
      case _ => aggOrError
    }) match {
      case Right(Seq(Inl(Field(name)), Inr(Inl(value)))) => Right(Map(name -> Seq(Some(value.toString.asJValue))))
      case Right(_) => Left("Something went wrong...")
      case Left(x) => Left(x)
    }

  private case class PartitionsTuple(partitionedTokens: Seq[Seq[Token]],
                                     previousToken: Token)
}
