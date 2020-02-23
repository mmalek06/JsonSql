package com.mmalek.jsonSql.execution.runnables.selectables.operators.base

import com.mmalek.jsonSql.execution.runnables.Folders.RunnableArgumentToNumber
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

abstract class BaseCalculationOperator extends Selectable {
  def run(allArgs: Seq[RunnableArgument], json: Option[JValue] = None): Option[(RunnableArgument, Int)] = {
    val args = allArgs.takeRight(2)
    val num1 = args.head.fold(RunnableArgumentToNumber)
    val num2 = args.last.fold(RunnableArgumentToNumber)

    (for {
      n1 <- num1
      n2 <- num2
    } yield Coproduct[RunnableArgument](operation(n1, n2))).map(r => (r, 2))
  }

  protected def operation(arg1: BigDecimal, arg2: BigDecimal): BigDecimal
}
