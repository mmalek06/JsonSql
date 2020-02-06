package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JBool, JNumber, JString}
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}
import shapeless.{:+:, CNil}

object Types {
  type RunnableArgument = Field :+: BigDecimal :+: String :+: CNil
  type FilterableArgument = Field :+: Constant :+: JString :+: JNumber :+: JBool :+: CNil
}
