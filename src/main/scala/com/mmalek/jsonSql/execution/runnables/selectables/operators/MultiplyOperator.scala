package com.mmalek.jsonSql.execution.runnables.selectables.operators

import com.mmalek.jsonSql.execution.runnables.Folders.{IsNumeric, RunnableArgumentToNumber}
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

class MultiplyOperator extends Selectable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "*" && args.length >= 2 && args.forall(_.fold(IsNumeric))

  def run(allArgs: Seq[RunnableArgument], json: Option[JValue] = None): Option[(RunnableArgument, Int)] = {
    val args = allArgs.takeRight(2)
    val num1 = args.head.fold(RunnableArgumentToNumber)
    val num2 = args.last.fold(RunnableArgumentToNumber)

    (for {
      n1 <- num1
      n2 <- num2
    } yield Coproduct[RunnableArgument](n1 * n2)).map(r => (r, 2))
  }
}
