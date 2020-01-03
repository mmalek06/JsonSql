package com.mmalek.jsonSql

import com.mmalek.jsonSql.jsonParsing.JsonParser
import com.mmalek.jsonSql.sqlParsing.Tokenizer

class Runner {
  def run(rawSql: String, rawJson: String) = {
    val tokens = Tokenizer.tokenize(rawSql)
    val json = JsonParser.parse(rawJson)

    None
  }
}
