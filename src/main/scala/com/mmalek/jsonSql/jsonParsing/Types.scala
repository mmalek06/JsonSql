package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JField, JValue}
import shapeless.{:+:, CNil}

object Types {
  type CreatorArgument = String :+: Double :+: BigInt :+: Boolean :+: JValue :+: Seq[JField] :+: Seq[JValue] :+: CNil
}
