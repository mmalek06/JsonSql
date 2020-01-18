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
        SELECT (Avg("object.nestedKey") + 2) * "object.anotherKey", SUM("object.unnestedKey"), "someKey", 'string constant'
        FROM ##json##
        WHERE "key" = 'value' OR "something" = 1"""
    val data = runJsonSql(query, json)

    println("ASDf")
  }
}
