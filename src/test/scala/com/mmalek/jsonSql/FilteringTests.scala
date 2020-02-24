package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JNumber, JString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilteringTests extends AnyFlatSpec with Matchers {
  "JsonSql" should "not select anything from a simple object when conditions are not met" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", AVG("age") AS "avgage", "fullname" FROM ##json## WHERE "id" = 0"""
    val Right(result) = runJsonSql(query, json)

    result("id") should be (List(None))
  }

  it should "select three fields from a simple object when conditions are met" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", "fullname" FROM ##json## WHERE "id" = 1"""
    val Right(result) = runJsonSql(query, json)

    result("id") should be (List(Some(JNumber(1))))
    result("age") should be (List(Some(JNumber(1))))
    result("fullname") should be (List(Some(JString("Raymond Mann"))))
  }

  it should "return error message when a function is present in the where clause" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", "fullname" FROM ##json## WHERE AVG("id") = 1"""
    val Left(error) = runJsonSql(query, json)

    error should be ("Running functions inside where clauses is not supported yet.")
  }

  it should "filter the input properly when a flat where clause is passed" in {
    val json = SampleJson.list
    val query = s"""
        SELECT "items.id" AS "id", AVG("items.age") AS "itemsage", AVG("items.id") AS "itemsid", (2 + 3) * (4 + 1) AS "something"
        FROM ##json##
        WHERE "items.age" = 15 AND "items.address.city" = 'Gdańsk' OR "items.isEmployee" = false
        """
    val Right(result) = runJsonSql(query, json)

    result("id") should be (List(None, None, None, None, Some(JNumber(5)), None, None, None, None, Some(JNumber(10))))
    result("itemsage") should be (List(Some(JNumber(3.4))))
    result("itemsid") should be (List(Some(JNumber(1.5))))
    result("something") should be (List(Some(JNumber(25))))
  }

  it should "filter the input properly, based on where clause that's comparing two properties of the same object" in {

  }
}
