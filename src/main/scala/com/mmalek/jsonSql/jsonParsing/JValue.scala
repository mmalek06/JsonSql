package com.mmalek.jsonSql.jsonParsing

// inspired by: https://github.com/lift/lift/blob/master/framework/lift-base/lift-json/src/main/scala/net/liftweb/json/JsonAST.scala
sealed abstract class JValue {
  type Values

  def apply(i: Int): JValue = JNothing
  def values: Values
  def children: Seq[JValue] = this match {
    case JObject(l) => l
    case JArray(l) => l
    case JField(_, v) => List(v)
    case _ => Nil
  }
}

//noinspection TypeAnnotation
case object JNothing extends JValue {
  type Values = None.type
  def values = None
}

//noinspection TypeAnnotation
case object JNull extends JValue {
  type Values = Null
  def values = null
}

case class JString(s: String) extends JValue {
  type Values = String
  def values: String = s
}

case class JDouble(num: Double) extends JValue {
  type Values = Double
  def values: Values = num
}

case class JInt(num: BigInt) extends JValue {
  type Values = BigInt
  def values: BigInt = num
}

case class JBool(value: Boolean) extends JValue {
  type Values = Boolean
  def values: Values = value
}

case class JField(name: String, value: JValue) extends JValue {
  type Values = (String, value.Values)
  def values = (name, value.values)
  override def apply(i: Int): JValue = value(i)
}

//noinspection TypeAnnotation
case class JObject(obj: Seq[JField]) extends JValue {
  type Values = Map[String, Any]
  def values = Map() ++ obj.map(_.values : (String, Any))

  override def equals(that: Any): Boolean = that match {
    case o: JObject => Set(obj.toArray: _*) == Set(o.obj.toArray: _*)
    case _ => false
  }
}

//noinspection TypeAnnotation
case class JArray(arr: Seq[JValue]) extends JValue {
  type Values = Seq[Any]
  def values = arr.map(_.values)
  override def apply(i: Int): JValue = arr(i)

  override def equals(that: Any): Boolean = that match {
    case a: JArray => Set(arr.toArray: _*) == Set(a.arr.toArray: _*)
    case _ => false
  }
}
