package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures._
import com.mmalek.jsonSql.jsonParsing.fsm.{State, StateMachine}
import com.mmalek.jsonSql.jsonParsing.fsm.State._
import shapeless.Poly1

import scala.language.postfixOps

object JsonParser {
  def parse(input: String): JValue = {
    val seed = getSeed
    val ParsingTuple(_, tree, _, _, _) = Normalizer
      .normalize(input)
      .foldLeft(seed)((aggregate, char) => {
        aggregate.stateMachine.next(char, aggregate.builder, aggregate.statesHistory).map(sm => {
          val debugValue = extractString(aggregate.builder, aggregate.stateMachine.state)
          val (navigation, function) = getActionTuple(aggregate.stateMachine.state, aggregate.builder)

          aggregate.builder.clear()
          aggregate.builder.append(char)

          val nextHistory = sm.state :: aggregate.statesHistory.toList

          createParsingTuple(sm, aggregate, nextHistory, navigation, function, debugValue)
        }).getOrElse({
          aggregate.builder.append(char)
          aggregate
        })
      })

    TreeRunner.run(tree)
  }

  private def getSeed =
    ParsingTuple(
      new StateMachine(State.Initial),
      JsonCreatorsTree.zero,
      Nil,
      new StringBuilder,
      Nil)

  private def extractString(sb: StringBuilder, state: State) =
    state match {
      case Initial | ReadObjectEnd | ReadArrayEnd => ""
      case ReadObject => "{"
      case ReadObjectKey => getCleanedPropertyKey(sb)
      case ReadArray => "["
      case ReadScalar => getCleanedScalar(sb)
    }

  private def getActionTuple(state: State, sb: StringBuilder) =
    state match {
      case ReadObjectEnd | ReadArrayEnd => (Navigation.Up, None)
      case ReadObject => (Navigation.Down, Some(getObject))
      case ReadObjectKey => (Navigation.Down, Some(getPropertyKey(getCleanedPropertyKey(sb))))
      case ReadArray => (Navigation.Down, Some(getArray))
      case ReadScalar => (Navigation.Up, Some(getScalar(getCleanedScalar(sb))))
      case _ => (Navigation.Stay, None)
    }

  private def createParsingTuple(sm: StateMachine,
                                 aggregate: ParsingTuple,
                                 history: Seq[State],
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

    ParsingTuple(sm, newTree, path, aggregate.builder, history)
  }

  private def updatePathObjects(currentTreePath: Seq[Node], newObjects: Seq[Node]) =
    currentTreePath
      .foldLeft((List.empty[Node], newObjects))((pair, _) => pair match {
        case (newPath, updater) => (newPath :+ updater.head, updater.tail) })
      ._1

  private def getCleanedPropertyKey(sb: StringBuilder) = {
    val value = sb.toString
    val firstQuoteIdx = value.indexOf('"')
    val secondQuoteIdx = value.length - value.reverse.indexOf('"')

    value.substring(firstQuoteIdx + 1, secondQuoteIdx - 1)
  }

  private def getCleanedScalar(sb: StringBuilder) = {
    val value = sb.toString
    val firstQuoteIdx = value.indexOf('"')
    val secondQuoteIdx = value.length - value.reverse.indexOf('"')

    if (firstQuoteIdx < 0) value.trim
    else value.substring(firstQuoteIdx + 1, secondQuoteIdx - 1)
  }

  private val getPropertyKey = (name: String) =>
    (arg: CreatorArgument) =>
      arg map CreatorArgumentToJValue select match {
        case None => JNull(0)
        case Some(value) => JField(name, value)
      }

  private val getScalar = (scalar: String) => (_: CreatorArgument) =>
    if (scalar.forall(_.isDigit)) JInt(BigInt(scalar))
    else scalar
      .toDoubleOption
      .map(d => JDouble(d))
      .getOrElse(scalar
        .toBooleanOption
        .map(b => JBool(b))
        .getOrElse(if (scalar == "null") JNull(0) else JString(scalar)))

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

  private case class ParsingTuple(stateMachine: StateMachine,
                                  tree: JsonCreatorsTree,
                                  currentTreePath: Seq[Node],
                                  builder: StringBuilder,
                                  statesHistory: Seq[State])
}
