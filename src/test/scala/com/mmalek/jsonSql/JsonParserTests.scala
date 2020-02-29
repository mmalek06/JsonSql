package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.JsonParser
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JArray, JBool, JField, JNumber, JObject, JString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JsonParserTests extends AnyFlatSpec with Matchers {
  "JsonParser" should "parse simple array correctly" in {
    val json = """[1, 2, true, "asdf"]"""
    val result = JsonParser.getJson(json)

    result should be (
      JArray(
        Seq(
          JNumber(1),
          JNumber(2),
          JBool(true),
          JString("asdf"))))
  }

  it should "parse arrays with object nested at N - 2 correctly" in {
    val json = """[1, 2, true, { "key": 1 }, "asdf"]"""
    val result = JsonParser.getJson(json)

    result should be (
      JArray(
        Seq(
          JNumber(1),
          JNumber(2),
          JBool(true),
          JObject(
            Seq(
              JField("key", JNumber(1)))),
          JString("asdf"))))
  }

  it should "parse arrays with object nested at N - 1 correctly" in {
    val json = """[1, 2, true, "asdf", { "key": 1 }]"""
    val result = JsonParser.getJson(json)

    result should be (
      JArray(
        Seq(
          JNumber(1),
          JNumber(2),
          JBool(true),
          JString("asdf"),
          JObject(
            Seq(
              JField("key", JNumber(1)))))))
  }

  it should "parse object with nested array and object correctly" in {
    val json =
      """
      {
          "items":[
              {
                  "id":1,
                  "address":{
                      "street": "Street3",
                      "city": "City2"
                  }
              },
              {
                  "id":3,
                  "age":30,
                  "name":"Robert",
                  "surname":"Cobb",
                  "fullname":"Jack Gibson",
                  "isEmployee":true,
                  "address":{
                      "street": "Street2",
                      "city": "City1"
                  }
              }
          ]
      }
      """
    val result = JsonParser.getJson(json)

    result should be (
      JObject(
        Seq(
          JField("items", JArray(
            Seq(
              JObject(
                Seq(
                  JField("id", JNumber(1)),
                  JField("address", JObject(
                    Seq(
                      JField("street", JString("Street3")),
                      JField("city", JString("City2"))))))),
              JObject(
                Seq(
                  JField("id", JNumber(3)),
                  JField("age", JNumber(30)),
                  JField("name", JString("Robert")),
                  JField("surname", JString("Cobb")),
                  JField("fullname", JString("Jack Gibson")),
                  JField("isEmployee", JBool(true)),
                  JField("address", JObject(
                    Seq(
                      JField("street", JString("Street2")),
                      JField("city", JString("City1")))))))))))))
  }

  it should "parse string with special characters correctly" in {
    val json = """
      {
        "d": "+ - * / { [ ,"
      }
      """
    val result = JsonParser.getJson(json)

    result should be (
      JObject(
        Seq(
          JField("d", JString("+ - * / { [ ,"))
        )))
  }

  it should "parse string with no whitespaces correctly" in {
    val json = """{"id":1,"age":1,"name":"Ralph","surname":"Garcia","fullname":"Raymond Mann","isEmployee":true,"address":{"street": "Street1","city": "City1"}}"""
    val result = JsonParser.getJson(json)

    result should be (
      JObject(
        Seq(
          JField("id", JNumber(1)),
          JField("age", JNumber(1)),
          JField("name", JString("Ralph")),
          JField("surname", JString("Garcia")),
          JField("fullname", JString("Raymond Mann")),
          JField("isEmployee", JBool(true)),
          JField("address",
            JObject(
              Seq(
                JField("street", JString("Street1")),
                JField("city", JString("City1"))))))))
  }

  it should "parse deeply nested arrays correctly" in {
    val json = """
      [
        [
          [
            [
              [
                { "key": 1 },
                { "key": 2 }
              ],
              [
                { "key": 1 },
                { "key": 2 }
              ]
            ]
          ]
        ]
      ]
      """
    val result = JsonParser.getJson(json)

    result should be (
      JArray(
        Seq(
          JArray(
            Seq(
              JArray(
                Seq(
                  JArray(
                    Seq(
                      JArray(
                        Seq(
                          JObject(
                            Seq(JField("key", JNumber(1)))),
                          JObject(
                            Seq(JField("key", JNumber(2)))))),
                      JArray(
                        Seq(
                          JObject(
                            Seq(
                              JField("key", JNumber(1)))),
                          JObject(
                            Seq(
                              JField("key", JNumber(2)))))))))))))))
  }

  it should "parse deeply nested objects correctly" in {
    val json = """
      {
        "a": {
          "b": {
            "c": {
              "d": {
                "e": [
                  { "key": 1 },
                  { "key": 2 }
                ],
                "f": [
                  { "key": 1 },
                  { "key": 2 }
                ]
              }
            }
          }
        }
      }
      """
    val result = JsonParser.getJson(json)

    result should be (
      JObject(
        Seq(
          JField("a", JObject(
            Seq(
              JField("b", JObject(
                Seq(
                  JField("c", JObject(
                    Seq(
                      JField("d", JObject(
                        Seq(
                          JField("e", JArray(
                            Seq(
                              JObject(
                                Seq(JField("key", JNumber(1)))),
                              JObject(
                                Seq(JField("key", JNumber(2))))))),
                            JField("f", JArray(
                              Seq(
                                JObject(
                                  Seq(JField("key", JNumber(1)))),
                                JObject(
                                  Seq(JField("key", JNumber(2))))))))))))))))))))))
  }

  it should "parse deeply nested objects with arrays correctly" in {
    val json =
      """
        [
          {
            "a": [
              {
                "b": [
                  { "key": 1 },
                  { "key": 2 },
                  [7, 8, 9],
                  [
                    [4, 5, 6],
                    { "key": 1 }
                  ]
                ]
              }
            ]
          }
        ]
        """
    val result = JsonParser.getJson(json)

    result should be (
      JArray(
        Seq(
          JObject(
            Seq(
              JField(
                "a", JArray(
                  Seq(
                    JObject(
                      Seq(
                        JField(
                          "b", JArray(
                            Seq(
                              JObject(
                                Seq(JField("key", JNumber(1)))),
                              JObject(
                                Seq(JField("key", JNumber(2)))),
                              JArray(Seq(JNumber(7), JNumber(8), JNumber(9))),
                              JArray(
                                Seq(
                                  JArray(Seq(JNumber(4), JNumber(5), JNumber(6))),
                                  JObject(
                                    Seq(JField("key", JNumber(1)))))))))))))))))))
  }
}
