package com.mmalek.jsonSql.execution.runnables.selectables.operators

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.operators.base.BaseCalculationOperator

class DivideOperator extends BaseCalculationOperator {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "/" && args.length >= 2

  protected def operation(arg1: BigDecimal, arg2: BigDecimal): BigDecimal = arg1 / arg2
}
