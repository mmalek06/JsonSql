package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.filterables.operators.EqualOperator
import com.mmalek.jsonSql.execution.runnables.selectables.functions.AvgFunction
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JArray, JBool, JField, JNothing, JNull, JNumber, JObject, JString, JValue}
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.Coproduct
import com.mmalek.jsonSql.extensions.JValueOps._

import scala.annotation.tailrec

object Filter {
  private val conjunctions = Set[Token](Or, And)
  private val operators = Seq(
    new EqualOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(filters: Seq[Token], json: JValue): Either[String, JValue] = {
    val rpn = Infix2RpnLogicalConverter.convert(filters)
    val maxPaths = calculateMaxPaths(rpn)

    maxPaths match {
      case Right(value) =>
        val partitions = partitionFilters(rpn)
//        val filterFunction = buildFilteringFunction(value, partitions)
//        val seed = Right(FilteringTuple(json, json, Seq.empty[RunnableArgument])).withLeft[String]
        //    val r = partitions.foldLeft(seed)((maybeAggregate, partition) => (maybeAggregate, partition) match {
        //      case (Right(aggregate), t@(And | Or) :: Nil) =>
        //        Right(aggregate)
        //      case (Right(aggregate), p) =>
        //        val currentArgs = getCurrentArguments(json, p)
        //        val newAggregate = getNewAggregate(aggregate, currentArgs)
        //
        //        newAggregate
        //      case (Left(x), _) => Left(x)
        //    })

        Right(json)
      case Left(error) => Left(error)
    }
  }

  private def calculateMaxPaths(filters: Seq[Token]) = {
    val groups = filters
      .flatMap {
        case Field(value) if value == "" => None
        case Field(value) => Some(value.split("\\.").toList)
        case _ => None
      }
      .groupBy(_.head)

    if (groups.size > 1) Left("Filtering expressions should all relate to one object group. Check the github docs on how to properly select items from more than one group.")
    else {
      val sorted = groups.head._2.sortBy(_.length)(Ordering.Int.reverse)
      val longest = sorted.head

      Right(sorted.tail.foldLeft(longest)((maxPath, array) => pickPath(maxPath, array, Nil).toList))
    }
  }

  @tailrec
  private def pickPath(maxPathSoFar: Seq[String], nextParts: Seq[String], calculatedPath: Seq[String]): Seq[String] =
    maxPathSoFar match {
      case Nil => calculatedPath
      case x :: xs =>
        if (nextParts.head != x) calculatedPath
        else pickPath(xs, nextParts.tail, calculatedPath :+ x)
    }

  private def buildFilteringFunction(maxPath: Seq[String], filterPartitions: Seq[Seq[Token]]) = {
    def func(json: JValue, rootsPath: Seq[String]): JValue =
      (json, rootsPath) match {
        case (JObject(fields), x :: xs) => JObject(fields.flatMap {
          case f if f.name == x => Some(JField(f.name, func(f, xs)))
          case _ => None
        })
        case (JArray(arr), path) => JArray(arr.map(func(_, path)))
        case (JObject(fields), Nil) =>
      }

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
    val seed = Right(Seq.empty[RunnableArgument]).withLeft[String]
    val currentArgs = partition.foldLeft(seed)((innerAggregate, t) => (innerAggregate, t) match {
      case (Right(aggregate), x: Constant) => Right(addConstantToArguments(aggregate, x))
      case (Right(aggregate), x: Field) => Right(addFieldValueToArguments(json, aggregate, x))
      case (Right(aggregate), op: Operator) => runOperator(operators, aggregate, json, op)
      case (Right(aggregate), x: Function) => Left("Running functions inside where clauses is not supported yet. Aborting...")
      case (x@Left(_), _) => x
      case _ => Left("Unsupported WHERE clause format. Aborting...")
    })

    currentArgs
  }

  private def addFieldValueToArguments(json: JValue, aggregate: Seq[RunnableArgument], x: Field) =
    aggregate :+ json.getValues(x.value.split("\\.")).flatten match {
      case Nil | JNull :: Nil => aggregate :+ Coproduct[RunnableArgument](())
      case JString(s) :: Nil => aggregate :+ Coproduct[RunnableArgument](s)
      case JNumber(num) :: Nil => aggregate :+ Coproduct[RunnableArgument](num)
      case JBool(value) :: Nil => aggregate :+ Coproduct[RunnableArgument](value)
    }

  private def runOperator(operators: Seq[Filterable], aggregate: Seq[RunnableArgument], json: JValue, x: Operator) =
    operators
      .find(_.canRun(x.value, aggregate))
      .map(value => {
        val result = value.run(aggregate, json)
        val newAggregate = aggregate.dropRight(2) :+ Coproduct[RunnableArgument](result)

        Right(newAggregate)
      })
      .getOrElse(Left(s"Couldn't run ${x.value} operator, because it is not a known filtering operator or the input was in bad format. Aborting..."))
}
