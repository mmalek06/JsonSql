package com.mmalek.jsonSql.execution.runnables.selectables.operators

import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.operators.base.BaseMultiComparisonOperator

class EqualOperator extends BaseMultiComparisonOperator {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "=" && args.length >= 2

  protected def operation(arg1: BigDecimal, arg2: BigDecimal): Boolean = arg1 == arg2

  protected def operation(arg1: String, arg2: String): Boolean = arg1 == arg2

  protected def operation(arg1: Boolean, arg2: Boolean): Boolean = arg1 == arg2
}
