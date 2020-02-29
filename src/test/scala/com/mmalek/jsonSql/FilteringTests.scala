package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JField, JNumber, JObject, JString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilteringTests extends AnyFlatSpec with Matchers {
  "JsonSql" should "return error message when a function is present in the where clause" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", "fullname" FROM ##json## WHERE AVG("id") = 1"""
    val Left(error) = runQuery(query, json)

    error should be ("Running functions inside where clauses is not supported yet.")
  }

  it should "return error message when the where clause statements use different object groups" in {
    val json = SampleJson.single
    val query = """SELECT "items.id", "items.age", "items.fullname" FROM ##json## WHERE "id" = 1 OR "something.what" = false"""
    val Left(error) = runQuery(query, json)

    error should be ("Filtering expressions should all relate to one object group. Check the github docs on how to properly select items from more than one group.")
  }

  it should "not select anything from a simple object when the where clause statement contains the wildcard symbol" in {
    val json = SampleJson.single
    val query = """SELECT "items.id" FROM ##json## WHERE "items.*" = 1"""
    val Right(result) = runQuery(query, json)

    result("items.id") should be (List(None))
  }

  it should "not select anything from a simple object when conditions are not met" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", AVG("age") AS "avgage", "fullname" FROM ##json## WHERE "id" = 0"""
    val Right(result) = runQuery(query, json)

    result("id") should be (List(None))
  }

  it should "select three fields from a simple object when conditions are met" in {
    val json = SampleJson.single
    val query = """SELECT "id", "age", "fullname" FROM ##json## WHERE "id" = 1"""
    val Right(result) = runQuery(query, json)

    result("id") should be (List(Some(JNumber(1))))
    result("age") should be (List(Some(JNumber(1))))
    result("fullname") should be (List(Some(JString("Raymond Mann"))))
  }

  it should "select two fields when using the wildcard symbol and when conditions are met" in {
    val json = SampleJson.list
    val query =
      """
        SELECT "items.id" AS "id", "items.address.*" AS "address"
        FROM ##json##
        WHERE "items.age" = 15
        """
    val Right(result) = runQuery(query, json)

    result("id") should be (Seq(None, None, None, None, None, None, Some(JNumber(7)), None, None, None))
    result("address") should be (
      Seq(None, None, None, None, None, None,
        Some(
          JObject(
            Seq(
              JField("street", JString("Street3")),
              JField("city", JString("City1"))))),
        None, None, None))
  }

  it should "filter the input properly when a flat where clause is passed" in {
    val json = SampleJson.list
    val query =
      """
        SELECT "items.id" AS "id", AVG("items.age") AS "itemsage", AVG("items.id") AS "itemsid", (2 + 3) * (4 + 1) AS "something"
        FROM ##json##
        WHERE "items.age" = 15 AND "items.address.city" = 'City1' OR "items.isEmployee" = false
        """
    val Right(result) = runQuery(query, json)

    result("id") should be (List(None, None, None, None, Some(JNumber(5)), None, Some(JNumber(7)), None, None, Some(JNumber(10))))
    result("itemsage") should be (List(Some(JNumber(4.9))))
    result("itemsid") should be (List(Some(JNumber(2.2))))
    result("something") should be (List(Some(JNumber(25))))
  }

  it should "filter the input properly when a bracketed where clause is passed" in {
    val json = SampleJson.list
    val query =
      """
        SELECT "items.id" AS "id"
        FROM ##json##
        WHERE ("items.age" > 10 AND "items.age" < 20 AND "items.address.city" = 'City1') OR "items.isEmployee" = false
        """
    val Right(result) = runQuery(query, json)

    result("id") should be (List(None, None, None, None, Some(JNumber(5)), None, Some(JNumber(7)), Some(JNumber(8)), None, Some(JNumber(10))))
  }

  it should "filter the input properly, based on where clause that's comparing two properties of the same object" in {
    val json = SampleJson.list
    val query =
      """
        SELECT "items.id" AS "id"
        FROM ##json##
        WHERE "items.age" = "items.id"
        """
    val Right(result) = runQuery(query, json)

    result("id") should be (List(None, None, None, None, Some(JNumber(5)), None, None, None, None, None))
  }
}
