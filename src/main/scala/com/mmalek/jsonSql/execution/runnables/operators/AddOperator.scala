package com.mmalek.jsonSql.execution.runnables.operators

import com.mmalek.jsonSql.execution.runnables.Folders._
import com.mmalek.jsonSql.execution.runnables.Runnable
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

class AddOperator extends Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "+" && args.length == 2 && (args.forall(_.fold(IsNumeric)) || args.forall(_.fold(IsString)))

  def run(args: Seq[RunnableArgument], json: Option[JValue] = None): Option[RunnableArgument] = {
    if (args.forall(_.fold(IsNumeric))) {
      val num1 = args.head.fold(RunnableArgumentToNumber)
      val num2 = args.last.fold(RunnableArgumentToNumber)

      for {
        n1 <- num1
        n2 <- num2
      } yield Coproduct[RunnableArgument](n1 + n2)
    } else {
      val num1 = args.head.fold(RunnableArgumentToString)
      val num2 = args.last.fold(RunnableArgumentToString)

      for {
        n1 <- num1
        n2 <- num2
      } yield Coproduct[RunnableArgument](n1 + n2)
    }
  }
}
