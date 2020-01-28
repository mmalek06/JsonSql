package com.mmalek.jsonSql

object App {
  def main(args: Array[String]): Unit = {
    val query =
      s"""
        SELECT AVG("items.age"), "items.age", "items.name"
        FROM ##json##
        """
    val data = runJsonSql(query, SampleJson.list)

    println("ASDf")
  }
}
