package com.mmalek.jsonSql.extensions

import com.mmalek.jsonSql.jsonParsing.dataStructures._

object StringOps {
  implicit class SqlParsingStringExtensions(val x: String) {
    def isNumber: Boolean =
      if (x.toIntOption.isDefined) true
      else x.toDoubleOption.isDefined

    def isString: Boolean =
      !isNumber && x.toBooleanOption.isEmpty

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
