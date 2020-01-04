package com.mmalek.jsonSql

object App {
  def main(args: Array[String]): Unit = {
    val json =
      """
      {
        "key": "value",
        "object": {
          "nestedKey": 1,
          "anotherKey": 2,
          "oneMore": "aaa"
        }
      }
    """
    val query =
      s"""
        SELECT object.nestedKey, object.anotherKey, object.nonExistent
        FROM ##json##
        WHERE key = 'value'"""
    val data = runJsonSql(query, json)

    println("ASDf")
  }
}
