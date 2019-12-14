package com.mmalek.jsonSql.sqlParsing.unitTests

import com.mmalek.jsonSql.sqlParsing.tokenization.Token._
import com.mmalek.jsonSql.sqlParsing.tokenization.Tokenizer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerTests extends AnyFlatSpec with Matchers {
  "A Tokenizer" should "parse simple input into tokens" in {
    val query = """SELECT thing FROM {"str_key": "value","int_key": 1} WHERE condition"""
    val result = Tokenizer.tokenize(query).toVector

    result.length should be (6)
    result(0) should be (Select)
    result(1) should be (Any("thing"))
    result(2) should be (From)
    result(3) should be (Json("""{"str_key": "value","int_key": 1}"""))
    result(4) should be (Where)
    result(5) should be (Any("condition"))
  }

  it should "parse even though text is not sql" in {
    val query = "lorem ipsum"
    val result = Tokenizer.tokenize(query).toVector

    result.length should be (2)
    result(0) should be (Any("lorem"))
    result(1) should be (Any("ipsum"))
  }
}
