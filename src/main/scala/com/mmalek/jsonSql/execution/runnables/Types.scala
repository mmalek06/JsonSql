package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}
import shapeless.{:+:, CNil}

object Types {
  type RunnableArgument = Field :+: Constant :+: Seq[Option[JValue]] :+: CNil
}
