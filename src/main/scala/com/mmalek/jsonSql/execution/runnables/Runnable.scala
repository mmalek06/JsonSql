package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument

trait Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean
  def run(symbol: String, args: Seq[RunnableArgument]): Unit
}
