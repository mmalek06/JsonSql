package com.mmalek.jsonSql.sqlParsing

import com.mmalek.jsonSql.sqlParsing.tokenization.Tokenizer


object SqlParser {
  def parse(input: String): Unit = {
    val tokens = Tokenizer.tokenize(input)

    println(tokens)
  }
}
