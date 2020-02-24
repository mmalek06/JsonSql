package com.mmalek.jsonSql

import com.mmalek.jsonSql.sqlParsing.Token._
import com.mmalek.jsonSql.sqlParsing.Tokenizer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerTests extends AnyFlatSpec with Matchers {
  "A Tokenizer" should "parse simple input into tokens" in {
    val query =
      """
        SELECT "thing" FROM ##json## WHERE "some.thing" = 1
        """.stripMargin
    val (tokens, error) = Tokenizer.tokenize(query)

    error should be (None)
    tokens.length should be (7)
    tokens.head should be (Select)
    tokens(1) should be (Field("thing"))
    tokens(2) should be (From)
    tokens(3) should be (Where)
    tokens(4) should be (Field("some.thing"))
    tokens(5) should be (Operator("="))
    tokens(6) should be (Constant("1"))
  }

  it should "parse complex input into tokens" in {
    val query =
      """
        SELECT "items.id" AS "id", AVG("items.age") AS "itemsage", AVG("items.id") AS "itemsid", (2 + 3) * (4 + 1) AS "something"
        FROM ##json##
        WHERE "items.age" > 15 AND "items.address.city" != 'Gdańsk' OR "items.isEmployee" = false OR "items.age" <= "items.id"
        """.stripMargin
    val (tokens, error) = Tokenizer.tokenize(query)

    error should be (None)
    tokens.length should be (46)
    tokens.head should be (Select)
    tokens(1) should be (Field("items.id"))
    tokens(2) should be (As)
    tokens(3) should be (FieldAlias("id"))
    tokens(4) should be (Function("avg"))
    tokens(5) should be (Bracket("("))
    tokens(6) should be (Field("items.age"))
    tokens(7) should be (Bracket(")"))
    tokens(8) should be (As)
    tokens(9) should be (FieldAlias("itemsage"))
    tokens(10) should be (Function("avg"))
    tokens(11) should be (Bracket("("))
    tokens(12) should be (Field("items.id"))
    tokens(13) should be (Bracket(")"))
    tokens(14) should be (As)
    tokens(15) should be (FieldAlias("itemsid"))
    tokens(16) should be (Bracket("("))
    tokens(17) should be (Constant("2"))
    tokens(18) should be (Operator("+"))
    tokens(19) should be (Constant("3"))
    tokens(20) should be (Bracket(")"))
    tokens(21) should be (Operator("*"))
    tokens(22) should be (Bracket("("))
    tokens(23) should be (Constant("4"))
    tokens(24) should be (Operator("+"))
    tokens(25) should be (Constant("1"))
    tokens(26) should be (Bracket(")"))
    tokens(27) should be (As)
    tokens(28) should be (FieldAlias("something"))
    tokens(29) should be (From)
    tokens(30) should be (Where)
    tokens(31) should be (Field("items.age"))
    tokens(32) should be (Operator(">"))
    tokens(33) should be (Constant("15"))
    tokens(34) should be (And)
    tokens(35) should be (Field("items.address.city"))
    tokens(36) should be (Operator("!="))
    tokens(37) should be (Constant("Gdańsk"))
    tokens(38) should be (Or)
    tokens(39) should be (Field("items.isEmployee"))
    tokens(40) should be (Operator("="))
    tokens(41) should be (Constant("false"))
    tokens(42) should be (Or)
    tokens(43) should be (Field("items.age"))
    tokens(44) should be (Operator("<="))
    tokens(45) should be (Field("items.id"))
  }
}
