package com.mmalek.jsonSql.execution.runnables.filterables.operators

import com.mmalek.jsonSql.execution.runnables.Folders.RunnableArgumentToBoolean
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.filterables.Filterable
import com.mmalek.jsonSql.execution.runnables.selectables.operators.{LesserThanOrEqualOperator => SelectableLtOrEqOp}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

class LesserThanOrEqualOperator extends Filterable {
  private val selectable = new SelectableLtOrEqOp

  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    selectable.canRun(symbol, args)

  def run(args: Seq[RunnableArgument], json: JValue): Boolean =
    selectable.run(args, None).exists(_._1.fold(RunnableArgumentToBoolean).get)
}
