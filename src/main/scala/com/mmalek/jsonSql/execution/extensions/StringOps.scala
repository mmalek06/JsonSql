package com.mmalek.jsonSql.execution.extensions

object StringOps {
  implicit class SqlParsingStringExtensions(val x: String) {
    def isNumber: Boolean =
      if (x.forall(_.isDigit)) true
      else x.toDoubleOption.isDefined
  }
}
