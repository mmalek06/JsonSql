package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

trait Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean
  def run(args: Seq[RunnableArgument], json: Option[JValue] = None): Option[RunnableArgument]
}
