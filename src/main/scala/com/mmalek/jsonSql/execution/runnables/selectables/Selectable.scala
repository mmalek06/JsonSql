package com.mmalek.jsonSql.execution.runnables.selectables

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

trait Selectable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean
  def run(args: Seq[RunnableArgument], json: Option[JValue]): Option[(RunnableArgument, Int)]

  case class SelectableResult(result: RunnableArgument, countArgsTaken: Int)
}
