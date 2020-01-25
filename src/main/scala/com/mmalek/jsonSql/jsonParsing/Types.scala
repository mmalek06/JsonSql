package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures._
import shapeless.{:+:, CNil}

object Types {
  type CreatorArgument = Unit :+: JString :+: JNumber :+: JBool :+: JObject :+: JArray :+: CNil
}
