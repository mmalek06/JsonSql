//package com.mmalek.jsonSql.jsonParsing.unitTests
//
//import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
//import com.mmalek.jsonSql.jsonParsing.dataStructures.{JString, JsonCreatorsTree}
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//class JsonCreatorsTreeTests extends AnyFlatSpec with Matchers {
//  "A node" should "return flat structure of its rightmost child" in {
//    val l1 = JsonCreatorsTree.zero.addChild((x: CreatorArgument) => JString("node"))
//    val l2 = l1.add
//  }
//}
