package com.mmalek.jsonSql.execution.runnables.selectables.operators

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.operators.base.BaseSingularComparisonOperator

class GreaterThanOperator extends BaseSingularComparisonOperator {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == ">" && args.length >= 2

  protected def operation(arg1: BigDecimal, arg2: BigDecimal): Boolean = arg1 > arg2
}
