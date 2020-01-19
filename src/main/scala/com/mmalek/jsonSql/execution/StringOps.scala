package com.mmalek.jsonSql.execution

object StringOps {
  implicit class SqlParsingStringExtensions(val x: String) {
    def isNumber: Boolean =
      if (x.forall(_.isDigit)) true
      else x.toDoubleOption.isDefined
  }
}
