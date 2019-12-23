package com.mmalek.jsonSql.jsonParsing.obsolete

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures._
import com.mmalek.jsonSql.jsonParsing.obsolete.extractors.{PropertyNameExtractor, ScalarValueExtractor}
import com.mmalek.jsonSql.jsonParsing.{Navigation, Normalizer}
import shapeless.Poly1

import scala.language.postfixOps

object JsonParser {
  def parse(input: String): JValue = {
    val seed = getSeed
    val ParsingTuple(tree, _, _, _) = Normalizer
      .normalize(input)
      .foldLeft(seed)((aggregate, char) => buildTree(aggregate, char))

    JNull(0)
  }

  private def getSeed =
    ParsingTuple(
      JsonCreatorsTree.zero,
      Nil,
      new PropertyNameExtractor(new StringBuilder),
      new ScalarValueExtractor(new StringBuilder))

  private def buildTree(aggregate: ParsingTuple, char: Char) = {
    val propertyNameExtractor = aggregate.propertyNameExtractor.next(char)
    val scalarValueExtractor = aggregate.scalarValueExtractor.next(char)
    val (navigation, maybeFunction, value, nextPropertyNameExtractor, nextScalarValueExtractor) =
      getActionTuple(char, propertyNameExtractor, scalarValueExtractor)
    val intermediateAggregate = aggregate.copy(
      propertyNameExtractor = nextPropertyNameExtractor,
      scalarValueExtractor = nextScalarValueExtractor)

    createParsingTuple(intermediateAggregate, navigation, maybeFunction, value.toString)
  }

  private def getActionTuple(char: Char,
                             nameExtractor: PropertyNameExtractor,
                             scalarExtractor: ScalarValueExtractor) =
    if (char == '{') getOpenObject(nameExtractor, scalarExtractor)
    else if (char == '[') getOpenArray(nameExtractor, scalarExtractor)
    else if (char == '"' && nameExtractor.isPropertyName) getPropertyName(nameExtractor, scalarExtractor)
    else if (scalarExtractor.isScalarValue) getScalarValue(nameExtractor, scalarExtractor)
    else if (char == '}' || char == ']') getLastObject(char, nameExtractor, scalarExtractor)
    else (Navigation.Stay, None, "", nameExtractor, scalarExtractor)

  private def getOpenObject(nameExtractor: PropertyNameExtractor, scalarExtractor: ScalarValueExtractor) = {
    val newNameExtractor = nameExtractor.flushBuilder.next('{')
    val (_, nextScalarExtractor) = scalarExtractor.flush
    val newScalarExtractor = nextScalarExtractor.next('{')

    (Navigation.Down, Some(getObject), "{", newNameExtractor, newScalarExtractor)
  }

  private def getOpenArray(nameExtractor: PropertyNameExtractor, scalarExtractor: ScalarValueExtractor) = {
    val newNameExtractor = nameExtractor.flushBuilder.next('[')
    val (_, nextScalarExtractor) = scalarExtractor.flush
    val newScalarExtractor = nextScalarExtractor.next('[')

    (Navigation.Down, Some(getArray), "[", newNameExtractor, newScalarExtractor)
  }

  private def getPropertyName(propertyExtractor: PropertyNameExtractor, scalarExtractor: ScalarValueExtractor) = {
    val (propertyName, newPropertyExtractor) = propertyExtractor.flush
    val (_, newScalarExtractor) = scalarExtractor.flush
    val getField = (arg: CreatorArgument) =>
      arg map CreatorArgumentToJValue select match {
        case None => JNull(0)
        case Some(value) => JField(propertyName, value)
      }

    (Navigation.Down, Option(getField), propertyName, newPropertyExtractor, newScalarExtractor)
  }

  private def getScalarValue(propertyExtractor: PropertyNameExtractor, scalarExtractor: ScalarValueExtractor) = {
    val (scalar, newScalarExtractor) = scalarExtractor.flush
    val newPropertyExtractor = propertyExtractor.flushBuilder.next(',')

    (Navigation.Up, Option((_: CreatorArgument) => scalar), scalar, newPropertyExtractor, newScalarExtractor)
  }

  private def getLastObject(char: Char, nameExtractor: PropertyNameExtractor, scalarExtractor: ScalarValueExtractor) = {
    (Navigation.Up, None, char.toString, nameExtractor, scalarExtractor)
  }

  private def createParsingTuple(aggregate: ParsingTuple,
                                 navigation: Navigation,
                                 f: Option[CreatorArgument => JValue],
                                 rawValue: String) = {
    val newTree = f.map(aggregate.tree.addChild(_, rawValue, aggregate.currentTreePath)).getOrElse(aggregate.tree)
    val rightmostPath = newTree.getRightmostChildPath()
    val oldPath = updatePathObjects(aggregate.currentTreePath, rightmostPath)
    val path =
      if (navigation == Navigation.Up) oldPath.init
      else if (navigation == Navigation.Stay) oldPath
      else rightmostPath

    ParsingTuple(newTree, path, aggregate.propertyNameExtractor, aggregate.scalarValueExtractor)
  }

  private def updatePathObjects(currentTreePath: Seq[Node], newObjects: Seq[Node]) =
    currentTreePath
      .foldLeft((List.empty[Node], newObjects))((pair, _) => pair match {
        case (newPath, updater) => (newPath :+ updater.head, updater.tail) })
      ._1

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
                                  currentTreePath: Seq[Node],
                                  propertyNameExtractor: PropertyNameExtractor,
                                  scalarValueExtractor: ScalarValueExtractor)
}
