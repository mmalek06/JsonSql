package com.mmalek.jsonSql.execution.runnables.filterables.operators

import com.mmalek.jsonSql.execution.runnables.Folders.{IsBoolean, IsNumeric, IsString, RunnableArgumentToBoolean}
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.selectables.operators.{NotEqualOperator => SelectableNotEqOp}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

class NotEqualOperator extends Filterable {
  private val selectable = new SelectableNotEqOp

  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    selectable.canRun(symbol, args)

  def run(allArgs: Seq[RunnableArgument], json: JValue): Boolean = {
    val args = allArgs.takeRight(2)

    if (args.forall(_.fold(IsNumeric))) selectable.runNumerics(args).exists(_._1.fold(RunnableArgumentToBoolean).get)
    else if (args.forall(_.fold(IsString))) selectable.runStrings(args).exists(_._1.fold(RunnableArgumentToBoolean).get)
    else if (args.forall(_.fold(IsBoolean))) selectable.runBools(args).exists(_._1.fold(RunnableArgumentToBoolean).get)
    else false
  }
}
