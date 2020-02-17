package com.mmalek.jsonSql.jsonParsing.dataStructures

//taken from: https://github.com/lift/lift/blob/master/framework/lift-base/lift-json/src/main/scala/net/liftweb/json/JsonAST.scala
sealed abstract class JValue {
  type Values
  def values: Values
}

//noinspection TypeAnnotation
case object JNothing extends JValue {
  override type Values = None.type
  override def values = None
}

//noinspection TypeAnnotation
case object JNull extends JValue {
  override type Values = Null
  override def values = null
}

case class JString(s: String) extends JValue {
  override type Values = String
  override def values: String = s
}

case class JNumber(num: BigDecimal) extends JValue {
  override type Values = BigDecimal
  override def values: BigDecimal = num
}

case class JBool(value: Boolean) extends JValue {
  override type Values = Boolean
  override def values: Values = value
}

//noinspection TypeAnnotation
case class JField(name: String, value: JValue) extends JValue {
  override type Values = (String, value.Values)
  override def values = (name, value.values)
}

//noinspection TypeAnnotation
case class JObject(fields: Seq[JField]) extends JValue {
  override type Values = Map[String, Any]
  override def values = Map() ++ fields.map(_.values : (String, Any))

  override def equals(that: Any): Boolean = that match {
    case o: JObject => Set(fields.toArray: _*) == Set(o.fields.toArray: _*)
    case _ => false
  }
}

//noinspection TypeAnnotation
case class JArray(arr: Seq[JValue]) extends JValue {
  override type Values = Seq[Any]
  override def values = arr.map(_.values)

  override def equals(that: Any): Boolean = that match {
    case a: JArray => Set(arr.toArray: _*) == Set(a.arr.toArray: _*)
    case _ => false
  }
}
