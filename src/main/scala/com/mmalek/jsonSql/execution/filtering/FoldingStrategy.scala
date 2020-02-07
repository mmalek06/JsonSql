package com.mmalek.jsonSql.execution.filtering

import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.functions.AvgFunction
import com.mmalek.jsonSql.execution.runnables.operators.{AddOperator, EqualOperator, GreaterThanOperator, NotEqualOperator}
import com.mmalek.jsonSql.execution.{getConstant, runFunction, runOperator}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.{Coproduct, Inl}

object FoldingStrategy {
  private val conjunctions = Set[Token](Or, And)
  private val operators = Seq(
    new AddOperator,
    new GreaterThanOperator,
    new EqualOperator,
    new NotEqualOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(filters: Seq[Token], json: JValue): JValue = {
    val rpn = Infix2RpnLogicalConverter.convert(filters)
    val partitions = partitionFilters(rpn)
    val seed = Right(FilteringTuple(json, json, Seq.empty[RunnableArgument])).withLeft[String]
    val r = partitions.foldLeft(seed)((maybeAggregate, partition) => (maybeAggregate, partition) match {
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

  private def getCurrentArguments(json: JValue, partition: Seq[Token]) = {
    val innerSeed = Right(Seq.empty[RunnableArgument]).withLeft[String]
    val currentArgs = partition.foldLeft(innerSeed)((innerAggregate, t) => (innerAggregate, t) match {
      case (Right(aggregate), x: Constant) => Right(getConstant(aggregate, x))
      case (Right(aggregate), x: Field) => Right(aggregate :+ Coproduct[RunnableArgument](x))
      case (Right(aggregate), op: Operator) => runOperator(operators, aggregate, op) match {
        case Right(Seq(Inl(_), x)) => Right(Seq(x))
        case Right(_) => Left(s"Operator ${op.value} returned invalid value during filtering. Aborting...")
        case x => x
      }
      case (Right(aggregate), x: Function) => runFunction(functions, aggregate, json, x) match {
        case Right(Seq(Inl(_), x)) => Right(Seq(x))
        case Right(_) => Left(s"Function ${x.name} returned invalid value during filtering. Aborting...")
        case x => x
      }
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
