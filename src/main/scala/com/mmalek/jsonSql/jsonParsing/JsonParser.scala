package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import com.mmalek.jsonSql.jsonParsing.extractors.{PropertyNameExtractor, ScalarValueExtractor}
import shapeless._

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
    val (navigation, maybeFunction, nextPropertyNameExtractor, nextScalarValueExtractor) =
      getActionTuple(char, propertyNameExtractor, scalarValueExtractor)

    maybeFunction
      .map(createParsingTuple(aggregate, navigation, nextPropertyNameExtractor, nextScalarValueExtractor, _))
      .getOrElse(aggregate.copy(propertyNameExtractor = nextPropertyNameExtractor, scalarValueExtractor = nextScalarValueExtractor))
  }

  private def getActionTuple(char: Char,
                             nameExtractor: PropertyNameExtractor,
                             scalarExtractor: ScalarValueExtractor) = {
    import shapeless.syntax.std.tuple._

    if (char == '{') (Navigation.Down, Some(getObject), nameExtractor, scalarExtractor)
    else if (char == '}') (Navigation.Up, None, nameExtractor, scalarExtractor)
    else if (char == '[') (Navigation.Down, Some(getArray), nameExtractor, scalarExtractor)
    else if (char == ']') (Navigation.Up, None, nameExtractor, scalarExtractor)
    else if (char == '"' && nameExtractor.isPropertyName) getPropertyName(nameExtractor) :+ scalarExtractor
    else if (scalarExtractor.isScalarValue) getScalarValue(nameExtractor, scalarExtractor)
    else (Navigation.Stay, None, nameExtractor, scalarExtractor)
  }

  private def createParsingTuple(aggregate: ParsingTuple,
                                 navigation: Navigation,
                                 propertyNameExtractor: PropertyNameExtractor,
                                 scalarValueExtractor: ScalarValueExtractor,
                                 f: CreatorArgument => JValue) = {
    val newTree = aggregate.tree.addChild(f)
    val currentPath = newTree.getRightmostChildPath()

    if (navigation == Navigation.Up) ParsingTuple(newTree, currentPath.init, propertyNameExtractor, scalarValueExtractor)
    else ParsingTuple(newTree, currentPath, propertyNameExtractor, scalarValueExtractor)
  }

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

  private def getPropertyName(nameExtractor: PropertyNameExtractor) = {
    val newExtractorTuple = nameExtractor.flush
    val getField = (arg: CreatorArgument) =>
      arg map CreatorArgumentToJValue select match {
        case None => JNull(0)
        case Some(value) => JField(newExtractorTuple._1, value)
      }

    (Navigation.Stay, Option(getField), newExtractorTuple._2)
  }

  private def getScalarValue(nameExtractor: PropertyNameExtractor, scalarExtractor: ScalarValueExtractor) = {
    val (scalar, newScalarExtractor) = scalarExtractor.flush
    val value =
      if (scalar.forall(_.isDigit)) JInt(BigInt(scalar))
      else scalar
        .toDoubleOption
        .map(d => JDouble(d))
        .getOrElse(scalar
          .toBooleanOption
          .map(b => JBool(b))
          .getOrElse(if (scalar == "null") JNull(0) else JString(scalar)))

    (Navigation.Stay, Option((_: CreatorArgument) => value), nameExtractor, newScalarExtractor)
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
