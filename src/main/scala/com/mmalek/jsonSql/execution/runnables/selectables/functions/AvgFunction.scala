package com.mmalek.jsonSql.execution.runnables.selectables.functions

import com.mmalek.jsonSql.execution.runnables.Folders.{IsField, RunnableArgumentToValueOption}
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.execution.runnables.selectables.Selectable
import com.mmalek.jsonSql.extensions.JValueOps._
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JNull, JNumber, JValue}
import shapeless.Coproduct

class AvgFunction extends Selectable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "avg" && args.nonEmpty && args.head.fold(IsField)

  def run(args: Seq[RunnableArgument], json: Option[JValue]): Option[(RunnableArgument, Int)] =
    if (json.isEmpty) None
    else
      args
        .last
        .fold(RunnableArgumentToValueOption)
        .map(fieldName => json.get.getValues(fieldName.split("\\.")))
        .flatMap(values => {
          if(hasInvalidValues(values)) None
          else {
            val numbers = values.flatten.map {
              case JNumber(v) => v
              case _ => BigDecimal(0)
            }

            if (numbers.isEmpty) None
            else Some(Coproduct[RunnableArgument](numbers.sum / numbers.length), 1)
          }
        })

  private def hasInvalidValues(values: Seq[Option[JValue]]) =
    values.exists {
      case Some(JNumber(_) | JNull) | None => false
      case _ => true
    }
}
