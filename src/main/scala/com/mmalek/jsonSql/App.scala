package com.mmalek.jsonSql

object App {
  def main(args: Array[String]): Unit = {
    val query =
      s"""
        SELECT "age", "name"
        FROM ##json##
        """
    val data = runJsonSql(query, SampleJson.single)

    println("ASDf")
  }
}
