package com.mmalek.jsonSql.execution.selectStrategies

import com.mmalek.jsonSql.execution.TokensInfo
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field, Operator}

object FoldingStrategy {
  def apply(tokens: Seq[Token], json: JValue, tokensInfo: TokensInfo): Map[String, Seq[Option[JValue]]] =
    ???
//    val nextTokens =
//      if(tokensInfo.hasOperators) rpn(tokens)
//      else tokens

//  private def rpn(tokens: Seq[Token]) =
//    tokens.filter {
//      case t: Operator | t: Field | t: Constant
//    }
}
