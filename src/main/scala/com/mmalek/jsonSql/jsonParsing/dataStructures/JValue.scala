package com.mmalek.jsonSql.jsonParsing.dataStructures

//taken from: https://github.com/lift/lift/blob/master/framework/lift-base/lift-json/src/main/scala/net/liftweb/json/JsonAST.scala
sealed trait JValue

case object JNothing extends JValue
case object JNull extends JValue
case class JString(s: String) extends JValue
case class JNumber(num: BigDecimal) extends JValue
case class JBool(value: Boolean) extends JValue
case class JField(name: String, value: JValue) extends JValue

case class JObject(fields: Seq[JField]) extends JValue {
  override def equals(that: Any): Boolean = that match {
    case o: JObject => Set(fields.toArray: _*) == Set(o.fields.toArray: _*)
    case _ => false
  }
}

case class JArray(arr: Seq[JValue]) extends JValue {
  override def equals(that: Any): Boolean = that match {
    case a: JArray => Set(arr.toArray: _*) == Set(a.arr.toArray: _*)
    case _ => false
  }
}
