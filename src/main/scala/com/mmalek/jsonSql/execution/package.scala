package com.mmalek.jsonSql

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.sqlParsing.Token.Constant
import shapeless.Coproduct

package object execution {
  def addConstantToArguments(aggregate: Seq[RunnableArgument], const: Constant): Seq[RunnableArgument] =
    if (const.value.isNumber) aggregate :+ Coproduct[RunnableArgument](BigDecimal(const.value))
    else if (const.value.isBoolean) aggregate :+ Coproduct[RunnableArgument](const.value.toBoolean)
    else aggregate :+ Coproduct[RunnableArgument](const.value)
}
