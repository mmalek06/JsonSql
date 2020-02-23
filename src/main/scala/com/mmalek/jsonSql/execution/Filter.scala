package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.runnables.Folders.RunnableArgumentToBoolean
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.filterables.operators.EqualOperator
import com.mmalek.jsonSql.execution.runnables.selectables.functions.AvgFunction
import com.mmalek.jsonSql.extensions.JValueOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures._
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.Coproduct

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
        val filterableJson = getFilterableJson(json, value)
        val transformedJson = eradicateNonMatching(partitions, filterableJson)

        transformedJson
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

  private def partitionFilters(tokens: Seq[Token]) =
    tokens.foldLeft(Seq.empty[Seq[Token]])((aggregate, t) => (aggregate, t) match {
      case (_, t) if conjunctions.contains(t) => aggregate :+ Seq(t)
      case (_ :+ (_ :+ Or), _) | (_ :+ (_ :+ And), _) => aggregate :+ Seq(t)
      case (Nil, _) => Seq(Seq(t))
      case _ => aggregate.init :+ (aggregate.last :+ t)
    })

  private def getFilterableJson(json: JValue, rootsPath: Seq[String]): JValue =
    (json, rootsPath) match {
      case (JObject(fields), x :: xs) => JObject(fields.flatMap {
        case field if field.name == x => Some(JField(field.name, getFilterableJson(field.value, xs)))
        case _ => None
      })
      case (JArray(arr), Nil) => JArray(arr.map(getFilterableJson(_, Nil)))
      case (JArray(arr), path) => JArray(arr.map(getFilterableJson(_, path)))
      case (x, Nil) => x
    }

  private def eradicateNonMatching(filterPartitions: Seq[Seq[Token]], filterableJson: JValue) =
    filterPartitions.foldLeft(Right(Seq.empty[RunnableArgument]).withLeft[String])((maybeAggregate, partition) => (maybeAggregate, partition) match {
      case (Right(x :: y :: _), And :: Nil) => runConjunction(x, y, _ && _)
      case (Right(x :: y :: _), Or :: Nil) => runConjunction(x, y, _ || _)
      case (Right(_), p) => flattenArguments(filterableJson, p)
      case (Left(x), _) => Left(x)
    }) match {
      case Right(x :: Nil) =>
        if (x.fold(RunnableArgumentToBoolean).getOrElse(false)) Right(filterableJson)
        else Right(JNull)
      case Left(error) => Left(error)
    }

  private def runConjunction(x: RunnableArgument, y: RunnableArgument, op: (Boolean, Boolean) => Boolean) =
    (for {
      arg1 <- x.fold(RunnableArgumentToBoolean)
      arg2 <- y.fold(RunnableArgumentToBoolean)
    } yield op(arg1, arg2)) match {
      case None => Left(s"Couldn't calculate boolean result from passed filter expressions. Expressions were: $x, $y. Aborting...")
      case Some(value) => Right(Seq(Coproduct[RunnableArgument](value)))
    }

  private def flattenArguments(json: JValue, partition: Seq[Token]) = {
    val seed = Right(Seq.empty[RunnableArgument]).withLeft[String]
    val currentArgs = partition.foldLeft(seed)((innerAggregate, t) => (innerAggregate, t) match {
      case (Right(aggregate), x: Constant) => Right(addConstantToArguments(aggregate, x))
      case (Right(aggregate), x: Field) => addFieldValueToArguments(json, aggregate, x)
      case (Right(aggregate), op: Operator) => runOperator(operators, aggregate, json, op)
      case (Right(_), _: Function) => Left("Running functions inside where clauses is not supported yet. Aborting...")
      case (x@Left(_), _) => x
      case _ => Left("Unsupported WHERE clause format. Aborting...")
    })

    currentArgs
  }

  private def addFieldValueToArguments(json: JValue, aggregate: Seq[RunnableArgument], x: Field) =
    json.getValues(x.value.split("\\.")).flatten match {
      case Nil | JNull :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](()))
      case JString(s) :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](s))
      case JNumber(num) :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](num))
      case JBool(value) :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](value))
      case x => Left(s"Scalar or null expected, $x found while parsing the filtering expression. Aborting...")
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
