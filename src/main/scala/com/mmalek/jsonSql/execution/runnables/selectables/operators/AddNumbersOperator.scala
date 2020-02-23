package com.mmalek.jsonSql.execution.runnables.selectables.operators

import com.mmalek.jsonSql.execution.runnables.Folders._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.operators.base.BaseCalculationOperator

class AddNumbersOperator extends BaseCalculationOperator {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "+" && args.length >= 2 && args.forall(_.fold(IsNumeric))

  override protected def operation(arg1: BigDecimal, arg2: BigDecimal): BigDecimal = arg1 + arg2
}
