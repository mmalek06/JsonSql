package com.mmalek.jsonSql.execution.runnables.filterables

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

trait Filterable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean
  def run(args: Seq[RunnableArgument], json: JValue): JValue

  case class SelectableResult(result: RunnableArgument, countArgsTaken: Int)
}
