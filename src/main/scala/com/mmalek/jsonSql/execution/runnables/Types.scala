package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.sqlParsing.Token.Field
import shapeless._
import shapeless.syntax.singleton._

object Types {
  val undefined = "undefined".narrow
  type RunnableArgument = Field :+: BigDecimal :+: String :+: Boolean :+: Unit :+: CNil
}
