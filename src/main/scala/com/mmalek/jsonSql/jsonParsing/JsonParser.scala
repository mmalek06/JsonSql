package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import com.mmalek.jsonSql.jsonParsing.extractors.PropertyNameExtractor
import shapeless._

import scala.language.postfixOps

object JsonParser {
  def parse(input: String): JValue = {
    val seed = ParsingTuple(JsonCreatorsTree.zero, PropertyNameExtractor(new StringBuilder))
    val result = Normalizer
      .normalize(input)
      .foldLeft(seed)((aggregate, char) => {
        val extractor = aggregate.propertyNameExtractor.next(char)
        val (maybeFunction, nextExtractor) = getFunctionBy(char, extractor)

        maybeFunction
          .map(f => ParsingTuple(aggregate.tree.addChild(f), nextExtractor))
          .getOrElse(ParsingTuple(aggregate.tree, nextExtractor))
      })

    JNull(0)
  }

  private def getFunctionBy(char: Char, nameExtractor: PropertyNameExtractor) =
    if (char == '{') (Some(getObject), nameExtractor)
    else if (char == '[') (Some(getArray), nameExtractor)
    else if (char == '"') maybeGetField(nameExtractor)
    else (None, nameExtractor)

  private val getObject = (arg: CreatorArgument) =>
    arg.select[Seq[JField]] match {
      case None => JNull(0)
      case Some(fields) => JObject(fields)
    }

  private val getArray = (arg: CreatorArgument) =>
    arg.select[Seq[JValue]] match {
      case None => JNull(0)
      case Some(array) => JArray(array)
    }

  private def maybeGetField(nameExtractor: PropertyNameExtractor) =
    if (nameExtractor.isProperty) {
      val newExtractorTuple = nameExtractor.flush

      (Some(getField(newExtractorTuple._1)), newExtractorTuple._2)
    } else (None, nameExtractor)

  private def getField(name: String) = (arg: CreatorArgument) =>
    arg map CreatorArgumentToJValue select match {
      case None => JNull(0)
      case Some(value) => JField(name, value)
    }

  object CreatorArgumentToJValue extends Poly1 {
    implicit val atString: Case.Aux[String, JValue] = at { x: String => JString(x) }
    implicit val atDouble: Case.Aux[Double, JValue] = at { x: Double => JDouble(x) }
    implicit val atBigInt: Case.Aux[BigInt, JValue] = at { x: BigInt => JInt(x) }
    implicit val atBoolean: Case.Aux[Boolean, JValue] = at { x: Boolean => JBool(x) }
    implicit val atJValue: Case.Aux[JValue, JValue] = at { x: JValue => x }
    implicit val atFields: Case.Aux[Seq[JField], JValue] = at { x: Seq[JField] => JObject(x) }
    implicit val atValues: Case.Aux[Seq[JValue], JValue] = at { x: Seq[JValue] => JArray(x) }
  }

  private case class ParsingTuple(tree: JsonCreatorsTree,
                                  propertyNameExtractor: PropertyNameExtractor)
}
