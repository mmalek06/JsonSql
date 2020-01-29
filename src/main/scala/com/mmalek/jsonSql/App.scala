package com.mmalek.jsonSql

object App {
  def main(args: Array[String]): Unit = {
    val query =
//      s"""
//        SELECT AVG("items.age") AS "avgAge", "items.age", "items.name"
//        FROM ##json##
//        """
      s"""
        SELECT "items.age" AS "age", "items.name", 33 AS "something"
        FROM ##json##
        """
    val data = runJsonSql(query, SampleJson.list)

    println("ASDf")
  }
}
