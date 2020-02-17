package com.mmalek.jsonSql.extensions

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JArray, JField, JNull, JObject, JValue}

object JValueOps {
  implicit class JValueExtensions(val json: JValue) {
    def getValues(path: Seq[String]): Seq[Option[JValue]] = walk(path, json)

    def getParentedValues(path: Seq[String]): JValue = walkParented(path, json)

    private def walk(path: Seq[String], x: JValue): Seq[Option[JValue]] =
      if (path.isEmpty) Seq(None)
      else x match {
        case JObject(fields) =>
          fields.find(_.name == path.head).map(_.value) match {
            case Some(value) if path.tail.isEmpty => Seq(Some(value))
            case Some(value) => walk(path.tail, value)
            case _ => Seq(None)
          }
        case JArray(arr) if path.tail.isEmpty =>
          arr.flatMap(v => walk(path, v))
        case JArray(arr) =>
          arr.flatMap(v => walk(path.tail, v))
        case _ =>
          Seq(Some(x))
      }

    def walkParented(path: Seq[String], x: JValue): JValue =
      if (path.isEmpty) JNull
      else x match {
        case JObject(fields) =>
          fields.find(_.name == path.head) match {
            case Some(field) if path.tail.isEmpty => JObject(Seq(field))
            case Some(field) =>
              JObject(Seq(JField(field.name, walkParented(path.tail, field.value))))
            case _ => JNull
          }
        case JArray(arr) => JArray(arr.map(v => walkParented(path, v)))
        case _ => x
      }
  }
}
