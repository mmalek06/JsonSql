package com.mmalek.jsonSql.execution.runnables.conjunctions

import com.mmalek.jsonSql.execution.runnables.Runnable
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

class OrConjunction extends Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean = ???

  def run(args: Seq[RunnableArgument], json: Option[JValue]): Option[(RunnableArgument, Int)] = ???
}
