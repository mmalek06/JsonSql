package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JBool, JNumber, JString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SelectionTests extends AnyFlatSpec with Matchers {
  "JsonSql" should "return an error message when a function call is not followed by an alias" in {
    val json = SampleJson.single
    val query = """SELECT Avg("age") FROM ##json##"""
    val Left(error) = runQuery(query, json)

    error should be ("FROM statement not present. The cause for this error may be that you forgot to alias function call in preceding SELECT clause.")
  }

  it should "select fields from flat json object" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", AVG("age") AS "avgage", "fullname" FROM ##json##"""
    val Right(result) = runQuery(query, json)

    result("id") should be (Seq(Some(JNumber(1))))
    result("age") should be (Seq(Some(JNumber(1))))
    result("avgage") should be (Seq(Some(JNumber(1))))
    result("fullname") should be (Seq(Some(JString("Raymond Mann"))))
  }

  it should "select fields from nested json object" in {
    val json = SampleJson.list
    val query = """
      SELECT "items.id" AS "id", AVG("items.age") AS "avgage", AVG("items.id") AS "avgid", (2 + 3) * (4 + 1) AS "something"
      FROM ##json##""".stripMargin
    val Right(result) = runQuery(query, json)

    result("id") should be (Seq(
      Some(JNumber(1)),
      Some(JNumber(2)),
      Some(JNumber(3)),
      Some(JNumber(4)),
      Some(JNumber(5)),
      Some(JNumber(6)),
      Some(JNumber(7)),
      Some(JNumber(8)),
      Some(JNumber(9)),
      Some(JNumber(10))))
    result("avgage") should be (Seq(Some(JNumber(15.125))))
    result("avgid") should be (Seq(Some(JNumber(5.5))))
    result("something") should be (Seq(Some(JNumber(25))))
  }

  it should "select fields properly, despite input json weirdness" in {
    val json = SampleJson.weird
    val query = """SELECT "d" FROM ##json##"""
    val Right(result) = runQuery(query, json)

    result("d") should be (Seq(Some(JString("+ - * /"))))
  }

  it should "select fields properly, despite no-whitespaces formatting" in {
    val json = SampleJson.noWhitespace
    val query = """SELECT "id", "age", "name", "surname", "fullname", "isEmployee", "address.street", "address.city" FROM ##json##"""
    val Right(result) = runQuery(query, json)

    result("id") should be (Seq(Some(JNumber(1))))
    result("age") should be (Seq(Some(JNumber(1))))
    result("name") should be (Seq(Some(JString("Ralph"))))
    result("surname") should be (Seq(Some(JString("Garcia"))))
    result("fullname") should be (Seq(Some(JString("Raymond Mann"))))
    result("isEmployee") should be (Seq(Some(JBool(true))))
    result("address.city") should be (Seq(Some(JString("City1"))))
    result("address.street") should be (Seq(Some(JString("Street1"))))
  }

  it should "select fields properly, despite string constant containing an escape character" in {
    val json = SampleJson.single
    val query = """SELECT "id", 'I\'m a constant!' AS "const" FROM ##json##"""
    val Right(result) = runQuery(query, json)

    result("id") should be (Seq(Some(JNumber(1))))
    result("const") should be (Seq(Some(JString("I'm a constant!"))))
  }

  it should "select fields properly from a nested array" in {
    val json = SampleJson.arrayOfArrays
    val query = """SELECT "key" FROM ##json##"""
    val Right(result) = runQuery(query, json)

    result("key") should be (Seq(Some(JNumber(1)), Some(JNumber(2)), Some(JNumber(1)), Some(JNumber(2))))
  }
}
