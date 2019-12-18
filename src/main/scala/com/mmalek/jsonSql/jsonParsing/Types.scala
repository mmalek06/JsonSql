package com.mmalek.jsonSql.jsonParsing

import shapeless.{:+:, CNil}

object Types {
  type CreatorArgument = String :+: Double :+: BigInt :+: Boolean :+: JValue :+: Seq[JField] :+: Seq[JValue] :+: CNil
}
