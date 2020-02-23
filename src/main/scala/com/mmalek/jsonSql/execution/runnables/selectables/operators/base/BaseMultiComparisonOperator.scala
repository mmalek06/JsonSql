package com.mmalek.jsonSql.execution.runnables.selectables.operators.base

import com.mmalek.jsonSql.execution.runnables.Folders._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

abstract class BaseMultiComparisonOperator extends Selectable {
  def run(allArgs: Seq[RunnableArgument], json: Option[JValue]): Option[(RunnableArgument, Int)] = {
    val args = allArgs.takeRight(2)

    if (args.forall(_.fold(IsNumeric))) runNumerics(args)
    else if (args.forall(_.fold(IsString))) runStrings(args)
    else if (args.forall(_.fold(IsBoolean))) runBools(args)
    else Some(Coproduct[RunnableArgument](false), 2)
  }

  def runNumerics(args: Seq[RunnableArgument]): Option[(RunnableArgument, Int)] = {
    (for {
      n1 <- args.head.fold(RunnableArgumentToNumber)
      n2 <- args.last.fold(RunnableArgumentToNumber)
    } yield Coproduct[RunnableArgument](operation(n1, n2))).map(r => (r, 2))
  }

  def runStrings(args: Seq[RunnableArgument]): Option[(RunnableArgument, Int)] = {
    (for {
      s1 <- args.head.fold(RunnableArgumentToString)
      s2 <- args.last.fold(RunnableArgumentToString)
    } yield Coproduct[RunnableArgument](operation(s1, s2))).map(r => (r, 2))
  }

  def runBools(args: Seq[RunnableArgument]): Option[(RunnableArgument, Int)] = {
    (for {
      b1 <- args.head.fold(RunnableArgumentToBoolean)
      b2 <- args.last.fold(RunnableArgumentToBoolean)
    } yield Coproduct[RunnableArgument](operation(b1, b2))).map(r => (r, 2))
  }

  protected def operation(arg1: BigDecimal, arg2: BigDecimal): Boolean
  protected def operation(arg1: String, arg2: String): Boolean
  protected def operation(arg1: Boolean, arg2: Boolean): Boolean
}
