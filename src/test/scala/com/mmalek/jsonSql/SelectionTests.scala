package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JNumber, JString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SelectionTests extends AnyFlatSpec with Matchers {
  "JsonSql" should "select fields from flat json object" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", AVG("age") AS "avgage", "fullname" FROM ##json##"""
    val Right(result) = runJsonSql(query, json)

    result("id") should be (Seq(Some(JNumber(1))))
    result("age") should be (Seq(Some(JNumber(1))))
    result("avgage") should be (Seq(Some(JNumber(1))))
    result("fullname") should be (Seq(Some(JString("Raymond Mann"))))
  }

  "JsonSql" should "select fields from nested json object" in {
    val json = SampleJson.list
    val query = """
      SELECT "items.id" AS "id", AVG("items.age") AS "avgage", AVG("items.id") AS "avgid", (2 + 3) * (4 + 1) AS "something"
      FROM ##json##""".stripMargin
    val Right(result) = runJsonSql(query, json)

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
    result("avgage") should be (Seq(Some(JNumber(15.75))))
    result("avgid") should be (Seq(Some(JNumber(5.5))))
    result("something") should be (Seq(Some(JNumber(25))))
  }
}
