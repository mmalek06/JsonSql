package com.mmalek.jsonSql.execution

import cats.implicits._
import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.runnables.Folders.RunnableArgumentToBoolean
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.filterables.operators._
import com.mmalek.jsonSql.extensions.JValueOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures._
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.Coproduct

import scala.annotation.tailrec

object Filter {
  private val conjunctions = Set[Token](Or, And)
  private val operators = Seq(
    new EqualOperator,
    new NotEqualOperator,
    new GreaterThanOperator,
    new GreaterThanOrEqualOperator,
    new LesserThanOperator,
    new LesserThanOrEqualOperator)

  def apply(filters: Seq[Token], json: JValue): Either[String, JValue] = {
    val rpn = Infix2RpnLogicalConverter.convert(filters)
    val maxPaths = calculateMaxPaths(rpn)

    maxPaths match {
      case Right(value) =>
        val partitions = partitionFilters(rpn)
        val filteredJson = filter(Right(json), value, partitions, Seq.empty[String])

        filteredJson
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

  // this works in two phases:
  // 1. it drills down the json tree as far, as the param rootsPath argument tells it to
  // 2. then it nulls objects not matching with whatever is inside filterPartitions argument
  private def filter(json: Either[String, JValue], rootsPath: Seq[String], filterPartitions: Seq[Seq[Token]], parentPath: Seq[String]): Either[String, JValue] = {
    def getFields(fields: Seq[JField], x: String, xs: Seq[String], parents: Seq[String]) =
      fields.toList.flatMap {
        case field if field.name == x =>
          filter(Right(field.value), xs, filterPartitions, parents) match {
            case Right(value) => Some(Right(JField(field.name, value)))
            case l@Left(_) => Some(l)
          }
        case _ => None
      }.sequence.map(r => r.map(_.asInstanceOf[JField]))

    (json, rootsPath, parentPath) match {
      case (l@Left(_), _, _) => l
      case (Right(JObject(fields)), x :: xs, parents) =>
        getFields(fields, x, xs, parents :+ x) match {
          case Right(value) => Right(JObject(value))
          case Left(error) => Left(error)
        }
      case (Right(JArray(arr)), path, parents) =>
        arr.map(x => filter(Right(x), path, filterPartitions, parents)).toList.sequence match {
          case Right(value) => Right(JArray(value))
          case Left(error) => Left(error)
        }
      case (Right(x), Nil, parents) => eradicateNonMatching(x, filterPartitions, parents)
    }
  }

  private def eradicateNonMatching(filterableJson: JValue, filterPartitions: Seq[Seq[Token]], parentPath: Seq[String]) =
    filterPartitions.foldLeft(Right(Seq.empty[RunnableArgument]).withLeft[String])((maybeAggregate, partition) => (maybeAggregate, partition) match {
      case (Right(x :: y :: _), And :: Nil) =>
        runConjunction(x, y, _ && _)
      case (Right(x :: y :: _), Or :: Nil) =>
        runConjunction(x, y, _ || _)
      case (Right(oldArgs), p) =>
        createArguments(filterableJson, p, parentPath) match {
          case l@Left(_) => l
          case Right(newArgs) => Right(oldArgs ++ newArgs)
        }
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
      case None => Left(s"Couldn't calculate boolean result from passed filter expressions. Expressions were: $x, $y.")
      case Some(value) => Right(Seq(Coproduct[RunnableArgument](value)))
    }

  private def createArguments(json: JValue, partition: Seq[Token], parentPath: Seq[String]) = {
    val seed = Right(Seq.empty[RunnableArgument]).withLeft[String]
    val currentArgs = partition.foldLeft(seed)((innerAggregate, t) => (innerAggregate, t) match {
      case (Right(aggregate), x: Constant) => Right(addConstantToArguments(aggregate, x))
      case (Right(aggregate), x: Field) => addFieldValueToArguments(json, aggregate, x, parentPath)
      case (Right(aggregate), op: Operator) => runOperator(operators, aggregate, json, op)
      case (Right(_), _: Function) => Left("Running functions inside where clauses is not supported yet.")
      case (x@Left(_), _) => x
      case _ => Left("Unsupported WHERE clause format.")
    })

    currentArgs
  }

  private def addFieldValueToArguments(json: JValue, aggregate: Seq[RunnableArgument], x: Field, parentPath: Seq[String]) =
    json.getValues(getValidPath(x, parentPath)).flatten match {
      case Nil | JNull :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](()))
      case JString(s) :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](s))
      case JNumber(num) :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](num))
      case JBool(value) :: Nil => Right(aggregate :+ Coproduct[RunnableArgument](value))
      case x => Left(s"Scalar or null expected, $x found while parsing the filtering expression.")
    }

  private def getValidPath(x: Field, parentPath: Seq[String]) =
    x.value.split("\\.").flatMap(step => if (parentPath.contains(step)) None else Some(step))

  private def runOperator(operators: Seq[Filterable], aggregate: Seq[RunnableArgument], json: JValue, x: Operator) =
    operators
      .find(_.canRun(x.value, aggregate))
      .map(value => {
        val result = value.run(aggregate, json)
        val newAggregate = aggregate.dropRight(2) :+ Coproduct[RunnableArgument](result)

        Right(newAggregate)
      })
      .getOrElse(Left(s"Couldn't run ${x.value} operator, because it is not a known filtering operator or the input was in bad format."))
}
