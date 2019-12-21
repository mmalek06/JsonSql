package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.JsonParser
import com.mmalek.jsonSql.jsonParsing.dataStructures.JsonCreatorsTree

object App {
  def main(args: Array[String]): Unit = {
//    val query =
//      """
//        SELECT thing
//        FROM {"str_key": "value","int_key": 1}
//        WHERE condition"""
//
//    SqlParser.parse(query)
//
//    val json =
//      """
//        {
//          "key": "value",
//          "object": {
//            "nestedObject": {
//              "nestedKey": 1
//            },
//            "nestedArray": [1, 3, 5]
//          },
//          "array": [
//            {
//              "arrayNestedObjectKey": "zxcv"
//            },
//            {
//              "arrayNestedObjectKey": "qwer"
//            }
//          ]
//        }
//      """
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
    JsonParser.parse(json)
//    val normalized = Normalizer.normalize(json)
//    val split = normalized.split(",")
//
//    println(split)
  }
}
