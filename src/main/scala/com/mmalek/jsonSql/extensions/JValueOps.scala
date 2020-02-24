package com.mmalek.jsonSql.extensions

import com.mmalek.jsonSql.jsonParsing.dataStructures.{JArray, JObject, JValue}

object JValueOps {
  implicit class JValueExtensions(val json: JValue) {
    def getValues(path: Seq[String]): Seq[Option[JValue]] = walk(path, json)

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
  }
}
