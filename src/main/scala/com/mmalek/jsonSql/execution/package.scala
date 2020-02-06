package com.mmalek.jsonSql

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token.{Function, Operator}

package object execution {
  def runOperator(operators: Seq[runnables.Runnable], aggregate: Seq[RunnableArgument], x: Operator): Either[String, Seq[RunnableArgument]] =
    operators
      .find(_.canRun(x.value, aggregate))
      .flatMap(_.run(aggregate))
      .map(value => Right(aggregate :+ value))
      .getOrElse(Left(s"Couldn't run ${x.value} operator, because the input was in bad format. Aborting..."))

  def runFunction(functions: Seq[runnables.Runnable], aggregate: Seq[RunnableArgument], json: JValue, x: Function): Either[String, Seq[RunnableArgument]] =
    functions
      .find(_.canRun(x.name, aggregate))
      .flatMap(_.run(aggregate, Some(json)))
      .map(value => Right(aggregate :+ value))
      .getOrElse(Left(s"Couldn't run ${x.name} function, because the input was in bad format. Aborting..."))
}
