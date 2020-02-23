package com.mmalek.jsonSql

object App {
  def main(args: Array[String]): Unit = {
    val query =
//      s"""
//        SELECT AVG("items.age") AS "avgAge", "items.age", "items.name" AS "name"
//        FROM ##json##
//        WHERE "items.address.city" != 'Gdańsk' OR "items.isEmployee" = false
//        """
      s"""
        SELECT "items.id" AS "id", AVG("items.age") AS "itemsage", AVG("items.id") AS "itemsid", (2 + 3) * (4 + 1) AS "something"
        FROM ##json##
        WHERE "items.age" = 15 AND "items.address.city" = 'Gdańsk' OR "items.isEmployee" = false OR "items.age" = "items.id"
        """
//      WHERE "items.address.city" = 'Gdynia' OR "items.age" = 16
//      s"""
//        SELECT (2 + 3) AS "avgAge1", AVG("items.age"), "items.age", "items.name" AS "name"
//        FROM ##json##
//        WHERE "items.age" = 15 AND "items.address.city" = 'Gdańsk' OR "items.isEmployee" = false AND true = true OR true = false
//        """
    //SELECT (2 + 3) * (4 + 1) AS "avgAge", AVG("items.age"), "items.age", "items.name" AS "name"
    //WHERE AVG("items.age") > 1 AND "items.address.city" != 'Gdańsk' OR "items.isEmployee" = false AND true = true OR true = false
    val data = runJsonSql(query, SampleJson.list)

    println("ASDf")
  }
}
