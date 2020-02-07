package com.mmalek.jsonSql

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Function, Operator}
import shapeless.Coproduct

package object execution {
  def runOperator(operators: Seq[runnables.Runnable], aggregate: Seq[RunnableArgument], x: Operator): Either[String, Seq[RunnableArgument]] =
    operators
      .find(_.canRun(x.value, aggregate))
      .flatMap(_.run(aggregate))
      .map(value => Right(aggregate :+ value))
      .getOrElse(Left(s"Couldn't run ${x.value} operator, because it is not a known runnable or the input was in bad format. Aborting..."))

  def runFunction(functions: Seq[runnables.Runnable], aggregate: Seq[RunnableArgument], json: JValue, x: Function): Either[String, Seq[RunnableArgument]] =
    functions
      .find(_.canRun(x.name, aggregate))
      .flatMap(_.run(aggregate, Some(json)))
      .map(value => Right(aggregate :+ value))
      .getOrElse(Left(s"Couldn't run ${x.name} function, because it is not a known runnable or the input was in bad format. Aborting..."))

  def getConstant(aggregate: Seq[RunnableArgument], const: Constant): Seq[RunnableArgument] =
    if (const.value.isNumber) aggregate :+ Coproduct[RunnableArgument](BigDecimal(const.value))
    else if (const.value.isBoolean) aggregate :+ Coproduct[RunnableArgument](const.value.toBoolean)
    else aggregate :+ Coproduct[RunnableArgument](const.value)
}
