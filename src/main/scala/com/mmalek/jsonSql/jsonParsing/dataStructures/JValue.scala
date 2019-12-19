package com.mmalek.jsonSql.jsonParsing.dataStructures

//taken from: https://github.com/lift/lift/blob/master/framework/lift-base/lift-json/src/main/scala/net/liftweb/json/JsonAST.scala
sealed abstract class JValue {
  type Values

  def apply(i: Int): JValue = JNothing
  def values: Values
  def children: Seq[JValue] = this match {
    case JObject(l) => l
    case JArray(l) => l
    case JField(_, v) => Seq(v)
    case _ => Nil
  }
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

case class JDouble(num: Double) extends JValue {
  override type Values = Double
  override def values: Values = num
}

case class JInt(num: BigInt) extends JValue {
  override type Values = BigInt
  override def values: BigInt = num
}

case class JBool(value: Boolean) extends JValue {
  override type Values = Boolean
  override def values: Values = value
}

//noinspection TypeAnnotation
case class JField(name: String, value: JValue) extends JValue {
  override type Values = (String, value.Values)
  override def values = (name, value.values)
  override def apply(i: Int): JValue = value(i)
}

//noinspection TypeAnnotation
case class JObject(obj: Seq[JField]) extends JValue {
  override type Values = Map[String, Any]
  override def values = Map() ++ obj.map(_.values : (String, Any))

  override def equals(that: Any): Boolean = that match {
    case o: JObject => Set(obj.toArray: _*) == Set(o.obj.toArray: _*)
    case _ => false
  }
}

//noinspection TypeAnnotation
case class JArray(arr: Seq[JValue]) extends JValue {
  override type Values = Seq[Any]
  override def values = arr.map(_.values)
  override def apply(i: Int): JValue = arr(i)

  override def equals(that: Any): Boolean = that match {
    case a: JArray => Set(arr.toArray: _*) == Set(a.arr.toArray: _*)
    case _ => false
  }
}
