package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JBool, JDouble, JField, JInt, JString, JValue}
import shapeless.{:+:, CNil}

object Types {
  type CreatorArgument = Unit :+: JString :+: JDouble :+: JInt :+: JBool :+: Seq[JField] :+: Seq[JValue] :+: CNil
}
