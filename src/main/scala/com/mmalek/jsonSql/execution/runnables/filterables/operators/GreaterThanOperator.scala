package com.mmalek.jsonSql.execution.runnables.filterables.operators

import com.mmalek.jsonSql.execution.runnables.Folders.RunnableArgumentToBoolean
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.selectables.operators.{GreaterThanOperator => SelectableGtThanOp}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

class GreaterThanOperator extends Filterable {
  private val selectable = new SelectableGtThanOp

  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    selectable.canRun(symbol, args)

  def run(args: Seq[RunnableArgument], json: JValue): Boolean =
    selectable.run(args, None).exists(_._1.fold(RunnableArgumentToBoolean).get)
}
