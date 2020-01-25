package com.mmalek.jsonSql.execution.extensions

object StringOps {
  implicit class SqlParsingStringExtensions(val x: String) {
    def isNumber: Boolean =
      if (x.toIntOption.isDefined) true
      else x.toDoubleOption.isDefined

    def isString: Boolean =
      !isNumber && x.toBooleanOption.isEmpty
  }
}
