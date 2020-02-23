package com.mmalek.jsonSql.execution.runnables.filterables.operators

import com.mmalek.jsonSql.execution.runnables.Folders._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.selectables.operators.{EqualOperator => SelectableEqOp}
import com.mmalek.jsonSql.jsonParsing.dataStructures._

class EqualOperator extends Filterable {
  private val selectable = new SelectableEqOp

  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    selectable.canRun(symbol, args)

  def run(allArgs: Seq[RunnableArgument], json: JValue): Boolean = {
    val args = allArgs.takeRight(2)

    if (args.forall(_.fold(IsNumeric))) selectable.runNumerics(args).exists(_._1.fold(RunnableArgumentToBoolean).get)
    else if (args.forall(_.fold(IsString))) selectable.runStrings(args).exists(_._1.fold(RunnableArgumentToBoolean).get)
    else if (args.forall(_.fold(IsBoolean))) selectable.runBools(args).exists(_._1.fold(RunnableArgumentToBoolean).get)
    //else if (args.exists(_.fold(IsField))) (runFields(args, json), 2, json)
    else true
  }

//  private def runFields(args: Seq[RunnableArgument], json: JValue, fieldMappedJsonVersions: Map[String, JValue]) = {
//    val argsWithIndices = args.zipWithIndex
//    val maybeArgs =
//      (for {
//        a1 <- argsWithIndices.find(_._1.fold(IsField))
//        jValues1 <- a1._1.fold(RunnableArgumentToValueOption)
//        a2 <- argsWithIndices.find(a => a._2 != a1._2)
//        maybeJValues2 = a2._1.fold(RunnableArgumentToValueOption)
//      } yield (jValues1, a2._1, maybeJValues2)).map {
//        case (fieldName1, _, Some(fieldName2)) if !fieldsRootedAtTheSameParent(fieldName1, fieldName2) =>
//          Left(s"Couldn't calculate equality of $fieldName1 and $fieldName2. In order to compare values, they need to belong to the same parent. Aborting...")
//        case (fieldName1, _, Some(fieldName2)) =>
//          val (longerName, shorterName) = if (fieldName1 >= fieldName2) (fieldName1, fieldName2) else (fieldName2, fieldName1)
//          val (longerPath, shorterPath) = getOrderedPaths(fieldName1, fieldName2)
//          val first = fieldMappedJsonVersions.get(shorterName)
//          val second = fieldMappedJsonVersions.get(longerName)
//          val pathUpToDivergence = longerPath.intersect(shorterPath)
//        case (fieldName, arg, _) =>
//          val version = fieldMappedJsonVersions.get(fieldName)
//          val strVal = arg.fold(RunnableArgumentToString)
//          val numVal = arg.fold(RunnableArgumentToNumber)
//          val boolVal = arg.fold(RunnableArgumentToBoolean)
//      }


//    val argsWithIndices = args.zipWithIndex
//    val maybeArgs =
//      for {
//        a1 <- argsWithIndices.find(_._1.fold(IsField))
//        jValues1 <- a1._1.fold(RunnableArgumentToValueOption).map(fieldName => json.getValues(fieldName.split("\\.")))
//        a2 <- argsWithIndices.find(a => a._2 != a1._2)
//      } yield (jValues1, a2._1)
//
//    maybeArgs.map(t => {
//      val (jValues1, arg2Wrapper) = t
//      val jValues2 = arg2Wrapper.fold(RunnableArgumentToValueOption).map(fieldName => json.getValues(fieldName.split("\\.")))
//      val strVal = arg2Wrapper.fold(RunnableArgumentToString)
//      val numVal = arg2Wrapper.fold(RunnableArgumentToNumber)
//      val boolVal = arg2Wrapper.fold(RunnableArgumentToBoolean)
//
//      (jValues2, strVal, numVal, boolVal) match {
//        case (Some(values), _, _, _) => compareJValues(jValues1, values)
//        case (_, Some(str), _, _) => compareJValuesToStrings(jValues1, str)
//        case (_, _, Some(num), _) => compareJValuesToNumbers(jValues1, num)
//        case (_, _, _, Some(bool)) => compareJValuesToBools(jValues1, bool)
//        case _ => jValues1.map(_ => false)
//      }
//    })
//  }

  private def fieldsRootedAtTheSameParent(fieldName1: String, fieldName2: String) = {
    val (longer, shorter) = getOrderedPaths(fieldName1, fieldName2)

    longer.containsSlice(shorter)
  }

  private def getOrderedPaths(fieldName1: String, fieldName2: String) = {
    val path1 = fieldName1.split("\\.")
    val path2 = fieldName2.split("\\.")
    val path1Len = path1.length
    val path2Len = path2.length

    if (path1Len >= path2Len) (path1, path2) else (path2, path1)
  }

  private def compareJValues(jValues1: Seq[Option[JValue]], s: Seq[Option[JValue]]) = {
    val first = jValues1.zipWithIndex
    val second = s.toVector

    first.map(row => (row._1, second(row._2)) match {
      case (Some(JNull), Some(JNull)) => true
      case (Some(JNull), _) => false
      case (_, Some(JNull)) => false
      case (Some(x), Some(y)) => x == y
      case (None, None) => true
      case _ => false
    })
  }

  private def compareJValuesToStrings(jValues1: Seq[Option[JValue]], str: String) =
    jValues1.map {
      case Some(JString(s)) => s == str
      case _ => false
    }

  private def compareJValuesToNumbers(jValues1: Seq[Option[JValue]], decimal: BigDecimal) =
    jValues1.map {
      case Some(JNumber(num)) => num == decimal
      case _ => false
    }

  private def compareJValuesToBools(jValues1: Seq[Option[JValue]], bool: Boolean) =
    jValues1.map {
      case Some(JBool(value)) => value == bool
      case _ => false
    }
}
