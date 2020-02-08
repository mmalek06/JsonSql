package com.mmalek.jsonSql.execution.runnables.operators

import com.mmalek.jsonSql.execution.runnables.Folders._
import com.mmalek.jsonSql.execution.runnables.Runnable
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

class EqualOperator extends Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "=" && args.length >= 2

  def run(allArgs: Seq[RunnableArgument], json: Option[JValue]): Option[(RunnableArgument, Int)] = {
    val args = allArgs.takeRight(2)

    if (args.forall(_.fold(IsNumeric)))
      (for {
        n1 <- args.head.fold(RunnableArgumentToNumber)
        n2 <- args.last.fold(RunnableArgumentToNumber)
      } yield Coproduct[RunnableArgument](n1 == n2)).map(r => (r, 2))
    else if (args.forall(_.fold(IsString)))
      (for {
        s1 <- args.head.fold(RunnableArgumentToString)
        s2 <- args.last.fold(RunnableArgumentToString)
      } yield Coproduct[RunnableArgument](s1 == s2)).map(r => (r, 2))
    else if (args.forall(_.fold(IsBoolean)))
      (for {
        b1 <- args.head.fold(RunnableArgumentToBoolean)
        b2 <- args.last.fold(RunnableArgumentToBoolean)
      } yield Coproduct[RunnableArgument](b1 == b2)).map(r => (r, 2))
    else Some(Coproduct[RunnableArgument](false), 2)
  }
}
