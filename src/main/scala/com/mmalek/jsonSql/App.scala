package com.mmalek.jsonSql

import com.mmalek.jsonSql.sqlParsing.SqlParser

object App {
  def main(args: Array[String]): Unit = {
    SqlParser.parse("SELECT a, avg(something_else) FROM jebs")
  }
}
