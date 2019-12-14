package com.mmalek.jsonSql.sqlParsing.unitTests

import com.mmalek.jsonSql.jsonParsing.normalization.Normalizer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NormalizerTests extends AnyFlatSpec with Matchers {
  "A Normalizer" should "normalize json text with whitespaces" in {
    val query =
      """{
           "str_key": "value",
           "int_key": 1
      }""".stripMargin
    val result = Normalizer.normalize(query)

    result should be ("{ \"str_key\": \"value\", \"int_key\": 1 }")
  }
}
