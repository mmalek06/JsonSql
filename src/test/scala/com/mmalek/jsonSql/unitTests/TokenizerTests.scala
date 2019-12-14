package com.mmalek.jsonSql.unitTests

import com.mmalek.jsonSql.sqlParsing.tokens.Token._
import com.mmalek.jsonSql.sqlParsing.tokens.Tokenizer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerTests extends AnyFlatSpec with Matchers{
  "A Tokenizer" should "parse simple input into tokens" in {
    val query = "SELECT thing FROM json_file WHERE condition"
    val result = Tokenizer.tokenize(query).toVector

    result.length should be (6)
    result(0) should be (Select)
    result(1) should be (Any("thing"))
    result(2) should be (From)
    result(3) should be (Any("json_file"))
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
