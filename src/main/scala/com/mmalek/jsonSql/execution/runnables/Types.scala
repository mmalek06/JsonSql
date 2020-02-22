package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.sqlParsing.Token.Field
import shapeless._

object Types {
  type RunnableArgument = Field :+: BigDecimal :+: String :+: Boolean :+: Unit :+: CNil
}
