package com.mmalek.jsonSql.execution.runnables.selectables.operators.base

import com.mmalek.jsonSql.execution.runnables.Folders.{IsNumeric, RunnableArgumentToNumber}
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

abstract class BaseSingularComparisonOperator extends Selectable {
  def run(allArgs: Seq[RunnableArgument], json: Option[JValue] = None): Option[(RunnableArgument, Int)] = {
    val args = allArgs.takeRight(2)

    if (args.forall(_.fold(IsNumeric)))
      (for {
        n1 <- args.head.fold(RunnableArgumentToNumber)
        n2 <- args.last.fold(RunnableArgumentToNumber)
      } yield Coproduct[RunnableArgument](operation(n1, n2))).map(r => (r, 2))
    else Some(Coproduct[RunnableArgument](false), 2)
  }

  protected def operation(arg1: BigDecimal, arg2: BigDecimal): Boolean
}
