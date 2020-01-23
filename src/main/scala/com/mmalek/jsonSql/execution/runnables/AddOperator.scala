package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.execution.JValueOps._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}

class AddOperator extends Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "+" && args.length == 2

  def run(symbol: String, args: Seq[RunnableArgument]): Unit =
    args.map {
      case Field(value) =>
      case Constant(value) =>
    }
}
