package com.mmalek.jsonSql.execution.filtering

import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.runnables.{AddOperator, AvgFunction}
import com.mmalek.jsonSql.execution.runnables.Types.{FilterableArgument, RunnableArgument}
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._
import shapeless.Coproduct
import com.mmalek.jsonSql.execution.{runFunction, runOperator}

object FoldingStrategy {
  private val logicalOperators = Set("=", ">", "<", "!=")
  private val conjunctions = Set[Token](Or, And)
  private val operators = Seq(
    new AddOperator)
  private val functions = Seq(
    new AvgFunction)

  def apply(filters: Seq[Token], json: JValue): JValue = {
    val rpn = Infix2RpnLogicalConverter.convert(filters)
    val partitions = partitionFilters(rpn)

//    partitions.foldLeft(FilteringTuple(json, json, Seq.empty[FilterableArgument]))((aggregate, partition) =>
//      partition.map {
//        case t@(Field(_) | Constant(_)) => aggregate.copy(currentArguments = aggregate.currentArguments :+ Coproduct[FilterableArgument](t))
//        case t@(Function(name)) => runFunction(functions, )
//      })

    json
  }

  private def partitionFilters(tokens: Seq[Token]) =
    tokens.foldLeft(Seq.empty[Seq[Token]])((aggregate, t) => (aggregate, t) match {
      case (_, Operator(op)) if logicalOperators.contains(op) => aggregate :+ Seq(t)
      case (_ :+ (_ :+ Operator(op)), _) if logicalOperators.contains(op) => aggregate :+ Seq(t)
      case (_, t) if conjunctions.contains(t) => aggregate :+ Seq(t)
      case (_ :+ (_ :+ Or), _) | (_ :+ (_ :+ And), _) => aggregate :+ Seq(t)
      case (Nil, _) => Seq(Seq(t))
      case _ => aggregate.init :+ (aggregate.last :+ t)
    })

  private case class FilteringTuple(initialJson: JValue,
                                    filteredJson: JValue,
                                    currentArguments: Seq[FilterableArgument])
}
