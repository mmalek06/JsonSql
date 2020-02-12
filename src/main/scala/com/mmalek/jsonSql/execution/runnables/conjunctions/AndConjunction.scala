package com.mmalek.jsonSql.execution.runnables.conjunctions

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue

class AndConjunction extends Selectable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean = ???

  def run(args: Seq[RunnableArgument], json: Option[JValue]): Option[(RunnableArgument, Int)] = ???
}
