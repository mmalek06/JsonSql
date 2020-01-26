package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JBool, JNumber, JNull, JString, JValue}

object StringOps {
  implicit class JsonParsingStringExtensions(val x: String) {
    def asJValue: JValue =
      if (!x.isEmpty && x.forall(_.isDigit)) JNumber(BigDecimal(x))
      else x
        .toDoubleOption
        .map(d => JNumber(BigDecimal(d)))
        .getOrElse(x
          .toBooleanOption
          .map(b => JBool(b))
          .getOrElse(if (x == "null") JNull else JString(x)))
  }
}
