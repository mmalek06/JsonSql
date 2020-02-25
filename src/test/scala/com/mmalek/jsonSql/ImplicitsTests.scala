package com.mmalek.jsonSql

import com.mmalek.jsonSql.implicits._
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JNumber, JString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ImplicitsTests extends AnyFlatSpec with Matchers {
  "A string" should "provide a query method when JsonSql is in scope" in {
    val json = SampleJson.single
    val Right(result) = json.query("""SELECT "id", "age", "fullname" FROM ##json## WHERE "id" = 1""")

    result("id") should be(List(Some(JNumber(1))))
    result("age") should be(List(Some(JNumber(1))))
    result("fullname") should be(List(Some(JString("Raymond Mann"))))
  }
}
