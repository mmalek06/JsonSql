package com.mmalek.jsonSql

import com.mmalek.jsonSql.sqlParsing.SqlParser

object App {
  def main(args: Array[String]): Unit = {
    val query =
      """
        SELECT thing
        FROM {"str_key": "value","int_key": 1}
        WHERE condition"""

    SqlParser.parse(query)
  }
}
