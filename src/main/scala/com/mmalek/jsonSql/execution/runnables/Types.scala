package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token.Field
import shapeless.{:+:, CNil}

object Types {
  type RunnableArgument = Field :+: BigDecimal :+: String :+: Seq[Option[JValue]] :+: CNil
}
