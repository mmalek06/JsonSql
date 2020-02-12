package com.mmalek.jsonSql.execution.filtering

import com.mmalek.jsonSql.execution.addConstantToArguments
import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.operators.EqualOperator
import com.mmalek.jsonSql.execution.runnables.selectables.functions.AvgFunction
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.Coproduct

object FoldingStrategy {
  private val conjunctions = Set[Token](Or, And)
  private val operators = Seq(
    new EqualOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(filters: Seq[Token], json: JValue): JValue = {
    val rpn = Infix2RpnLogicalConverter.convert(filters)
    val partitions = partitionFilters(rpn)
    val seed = Right(FilteringTuple(json, json, Seq.empty[RunnableArgument])).withLeft[String]
    val r = partitions.foldLeft(seed)((maybeAggregate, partition) => (maybeAggregate, partition) match {
      case (Right(aggregate), t@(And | Or) :: Nil) =>
        Right(aggregate)
      case (Right(aggregate), p) =>
        val currentArgs = getCurrentArguments(json, p)
        val newAggregate = getNewAggregate(aggregate, currentArgs)

        newAggregate
      case (Left(x), _) => Left(x)
    })

    json
  }

  private def partitionFilters(tokens: Seq[Token]) =
    tokens.foldLeft(Seq.empty[Seq[Token]])((aggregate, t) => (aggregate, t) match {
      case (_, t) if conjunctions.contains(t) => aggregate :+ Seq(t)
      case (_ :+ (_ :+ Or), _) | (_ :+ (_ :+ And), _) => aggregate :+ Seq(t)
      case (Nil, _) => Seq(Seq(t))
      case _ => aggregate.init :+ (aggregate.last :+ t)
    })

  // it's not my fault that the result of this method will be a sequence of partitions built from a partition.
  // also: partitionPartition is an awesome name! :)
  private def partitionPartition(partition: Seq[Token]) =
    partition.foldLeft(Seq.empty[Seq[Token]])((aggregate, t) => t match {
      case x@(Function(_) | Operator(_)) => aggregate.init :+ (aggregate.last :+ x) :+ Seq[Token]()
      case x => aggregate.init :+ (aggregate.last :+ x)
    })

  private def getCurrentArguments(json: JValue, partition: Seq[Token]) = {
    val innerSeed = Right(Seq.empty[RunnableArgument]).withLeft[String]
    val currentArgs = partition.foldLeft(innerSeed)((innerAggregate, t) => (innerAggregate, t) match {
      case (Right(aggregate), x: Constant) => Right(addConstantToArguments(aggregate, x))
      case (Right(aggregate), x: Field) => Right(aggregate :+ Coproduct[RunnableArgument](x))
      case (Right(aggregate), op: Operator) =>
        //runOperator(operators, aggregate, json, op)
      case (Right(aggregate), x: Function) =>
        //runFunction(functions, aggregate, json, x)
      case (x@Left(_), _) => x
      case _ => Left("Unsupported WHERE clause format. Aborting...")
    })

    currentArgs
  }

  private def getNewAggregate(aggregate: FilteringTuple, currentArgs: Either[String, Seq[RunnableArgument]]): Either[String, FilteringTuple] =
    currentArgs match {
      case Right(args) => Right(aggregate.copy(currentArguments = args))
      case Left(x) => Left(x)
    }

  private case class FilteringTuple(initialJson: JValue,
                                    filteredJson: JValue,
                                    currentArguments: Seq[RunnableArgument])
}
