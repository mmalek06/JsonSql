package com.mmalek.jsonSql.execution

import com.mmalek.jsonSql.execution.rpn.Infix2RpnLogicalConverter
import com.mmalek.jsonSql.execution.selectStrategies.{FoldingStrategy, MappingStrategy}
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JNull, JValue}
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token._

class SelectExecutor(actions: Map[Token, Seq[Token]]) {
  private val logicalOperators = Set("=", ">", "<", "!=")
  private val conjunctions = Set[Token](Or, And)

  def select(json: JValue): Either[String, Map[String, Seq[Option[JValue]]]] = {
    val tokens = actions(Select)
    val info = getSelectionInfo(tokens)

    if (info.hasFunctions || info.hasOperators) selectStrategies.FoldingStrategy(tokens, json)
    else MappingStrategy(tokens, json)
  }

  def from(): Option[Json] =
    actions(From).head match {
      case token: Json => Some(token)
      case _ => None
    }

  def where(json: JValue): JValue =
    actions.get(Where).map(filterJson(_, json)).getOrElse(JNull)

  private def getSelectionInfo(tokens: Seq[Token]) =
    tokens.foldLeft(SelectionTokensInfo(hasOperators = false, hasFunctions = false))((aggregate, t) => t match {
      case _: Operator => aggregate.copy(hasOperators = true)
      case Token.Function(_) => aggregate.copy(hasFunctions = true)
      case _ => aggregate
    })

  private def filterJson(filters: Seq[Token], json: JValue): JValue = {
    val rpn = Infix2RpnLogicalConverter.convert(filters)

//    val partitions = partitionFilters(filters)
//
//    partitions.zipWithIndex.foldLeft
//
//    partitions.zipWithIndex.map(p => {
//      val (partition, index) = p
//      val info = getFilteringInfo(partition)
//
//      if (info.hasFunctions || info.hasOperators) FoldingStrategy(p, json)
//      else if (info.hasLogicalOperators)
//      else {
//
//      }
//    })

    filters.flatMap {
      case t: Field => Some(t)
      case t: Constant => Some(t)
      case t: Operator => Some(t)
      case _ => None
    } match {
      case Nil => json
      case _ => json
    }
  }

  private def getFilteringInfo(tokens: Seq[Token]) =
    tokens.foldLeft(FilteringTokensInfo(hasOperators = false, hasFunctions = false, hasLogicalOperators = false, hasConjunction = false))((aggregate, t) => t match {
      case Operator(op) if !logicalOperators.contains(op) => aggregate.copy(hasOperators = true)
      case Operator(op) if logicalOperators.contains(op) => aggregate.copy(hasLogicalOperators = true)
      case Or | And => aggregate.copy(hasConjunction = true)
      case Token.Function(_) => aggregate.copy(hasFunctions = true)
      case _ => aggregate
    })

  private def partitionFilters(tokens: Seq[Token]) =
    tokens.foldLeft(Seq.empty[Seq[Token]])((aggregate, t) => (aggregate, t) match {
      case (_, Operator(op)) if logicalOperators.contains(op) => aggregate :+ Seq(t)
      case (_ :+ (_ :+ Operator(op)), _) if logicalOperators.contains(op) => aggregate :+ Seq(t)
      case (_, t) if conjunctions.contains(t) => aggregate :+ Seq(t)
      case (_ :+ (_ :+ Or), _) | (_ :+ (_ :+ And), _) => aggregate :+ Seq(t)
      case (Nil, _) => Seq(Seq(t))
      case _ => aggregate.init :+ (aggregate.last :+ t)
    })

  case class SelectionTokensInfo(hasOperators: Boolean, hasFunctions: Boolean)

  case class FilteringTokensInfo(hasOperators: Boolean,
                                 hasFunctions: Boolean,
                                 hasLogicalOperators: Boolean,
                                 hasConjunction: Boolean)
}
