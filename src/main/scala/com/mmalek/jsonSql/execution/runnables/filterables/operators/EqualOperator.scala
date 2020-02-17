package com.mmalek.jsonSql.execution.runnables.filterables.operators

import com.mmalek.jsonSql.execution.runnables.Folders._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.selectables.operators.{EqualOperator => SelectableEqOp}
import com.mmalek.jsonSql.extensions.JValueOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures._

class EqualOperator extends Filterable {
  private val selectable = new SelectableEqOp

  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    selectable.canRun(symbol, args)

  def run(allArgs: Seq[RunnableArgument], json: JValue): JValue = {
    val args = allArgs.takeRight(2)

    if (args.forall(_.fold(IsNumeric))) selectable.runNumerics(args).map(_ => json).getOrElse(JNull)
    else if (args.forall(_.fold(IsString))) selectable.runStrings(args).map(_ => json).getOrElse(JNull)
    else if (args.forall(_.fold(IsBoolean))) selectable.runBools(args).map(_ => json).getOrElse(JNull)
    //else if (args.exists(_.fold(IsField))) (runFields(args, json), 2, json)
    else JNull
  }

  private def runFields(args: Seq[RunnableArgument], json: JValue) = {
    val argsWithIndices = args.zipWithIndex
    val maybeArgs =
      for {
        a1 <- argsWithIndices.find(_._1.fold(IsField))
        jValues1 <- a1._1.fold(RunnableArgumentToValueOption).map(fieldName => json.getValues(fieldName.split("\\.")))
        a2 <- argsWithIndices.find(a => a._2 != a1._2)
      } yield (jValues1, a2._1)

    maybeArgs.map(t => {
      val (jValues1, arg2Wrapper) = t
      val jValues2 = arg2Wrapper.fold(RunnableArgumentToValueOption).map(fieldName => json.getValues(fieldName.split("\\.")))
      val strVal = arg2Wrapper.fold(RunnableArgumentToString)
      val numVal = arg2Wrapper.fold(RunnableArgumentToNumber)
      val boolVal = arg2Wrapper.fold(RunnableArgumentToBoolean)

      (jValues2, strVal, numVal, boolVal) match {
        case (Some(values), _, _, _) => compareJValues(jValues1, values)
        case (_, Some(str), _, _) => compareJValuesToStrings(jValues1, str)
        case (_, _, Some(num), _) => compareJValuesToNumbers(jValues1, num)
        case (_, _, _, Some(bool)) => compareJValuesToBools(jValues1, bool)
        case _ => jValues1.map(_ => false)
      }
    })
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
