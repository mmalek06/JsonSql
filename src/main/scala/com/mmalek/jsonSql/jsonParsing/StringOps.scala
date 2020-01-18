package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JBool, JDouble, JInt, JNull, JString, JValue}

object StringOps {
  implicit class StringExtensions(val x: String) {
    def asJValue: JValue =
      if (x.forall(_.isDigit)) JInt(BigInt(x))
      else x
        .toDoubleOption
        .map(d => JDouble(d))
        .getOrElse(x
          .toBooleanOption
          .map(b => JBool(b))
          .getOrElse(if (x == "null") JNull else JString(x)))
  }
}
