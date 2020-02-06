package com.mmalek.jsonSql

object App {
  def main(args: Array[String]): Unit = {
    val query =
//      s"""
//        SELECT AVG("items.age") AS "avgAge", "items.age", "items.name" AS "name"
//        FROM ##json##
//        WHERE "items.address.city" != 'Gdańsk' OR "items.isEmployee" = false
//        """
//      s"""
//        SELECT "items.age" AS "age", "items.name", 33 AS "something"
//        FROM ##json##
//        """
      s"""
        SELECT (2 + 3) * (4 - 1) AS "avgAge", "items.age", "items.name" AS "name"
        FROM ##json##
        WHERE AVG("items.age") > 1 AND "items.address.city" != 'Gdańsk' OR "items.isEmployee" = false AND true = true
        """
    val data = runJsonSql(query, SampleJson.list)

    println("ASDf")
  }
}
