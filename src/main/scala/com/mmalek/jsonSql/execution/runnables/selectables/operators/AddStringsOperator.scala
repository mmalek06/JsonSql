package com.mmalek.jsonSql.execution.runnables.selectables.operators

import com.mmalek.jsonSql.execution.runnables.Folders.{IsString, RunnableArgumentToString}
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import shapeless.Coproduct

class AddStringsOperator extends Selectable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "+" && args.length >= 2 && args.forall(_.fold(IsString))

  override def run(allArgs: Seq[RunnableArgument], json: Option[JValue] = None): Option[(RunnableArgument, Int)] ={
    val args = allArgs.takeRight(2)
    val str1 = args.head.fold(RunnableArgumentToString)
    val str2 = args.last.fold(RunnableArgumentToString)

    (for {
      s1 <- str1
      s2 <- str2
    } yield Coproduct[RunnableArgument](s1 + s2)).map(r => (r, 2))
  }
}
