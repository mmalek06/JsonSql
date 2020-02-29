package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.extensions.StringOps._
import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.NodeKind._
import com.mmalek.jsonSql.jsonParsing.dataStructures._
import com.mmalek.jsonSql.jsonParsing.fsm.State._
import com.mmalek.jsonSql.jsonParsing.fsm.{State, StateMachine}
import shapeless.Poly1

import scala.language.postfixOps

object JsonParser {
  def getJson(input: String): JValue = {
    val seed = getSeed
    val dontAppendOnTokens: Set[State] = Set(ReadArray, ReadArrayEnd, ReadObject, ReadObjectEnd)
    val ParsingTuple(_, tree, _, _, _) = Normalizer
      .normalize(input)
      .foldLeft(seed)((aggregate, char) => {
        aggregate.stateMachine.next(char, aggregate.builder, aggregate.statesHistory).map(sm => {
          val actionTuple = getActionTuple(aggregate.stateMachine.state, aggregate.builder, aggregate.statesHistory)

          aggregate.builder.clear()

          if (!dontAppendOnTokens.contains(sm.state)) aggregate.builder.append(char)

          val nextHistory = sm.state +: aggregate.statesHistory

          createParsingTuple(sm, aggregate, nextHistory, actionTuple)
        }).getOrElse({
          aggregate.builder.append(char)
          aggregate
        })
      })

    tree.execute
  }

  private def getSeed =
    ParsingTuple(
      new StateMachine(State.Initial),
      FunctionsTree.zero,
      Nil,
      new StringBuilder,
      Nil)

  private def getActionTuple(state: State, sb: StringBuilder, history: Seq[State]) =
    state match {
      case ReadObjectEnd | ReadArrayEnd if isInKeyNode(history) =>
        (Navigation.Up2, NoneNode, None)
      case ReadObjectEnd | ReadArrayEnd => (Navigation.Up, NoneNode, None)
      case ReadObject => (Navigation.Down, ObjectNode, Some(getObject))
      case ReadObjectKey => (Navigation.Down, KeyNode(sb.toString), Some(getPropertyKey(getCleanedPropertyKey(sb))))
      case ReadArray => (Navigation.Down, ArrayNode, Some(getArray))
      case ReadScalar if isInKeyNode(history) => (Navigation.Up2, ScalarNode(sb.toString), Some(getScalar(getCleanedScalar(sb))))
      case ReadScalar => (Navigation.Stay, ScalarNode(sb.toString), Some(getScalar(getCleanedScalar(sb))))
      case Initial => (Navigation.Stay, NoneNode, None)
      case _ => (Navigation.Stay, KeyNode(sb.toString), None)
    }

  private def isInKeyNode(history: Seq[State]) = {
    def trim2(openState: State, closeState: State) =
      history.foldLeft((Seq.empty[State], false, 0))((aggregate, state) => {
        val (trimmedHistory, foundOpener, openBracketsCnt) = aggregate

        if (foundOpener) (trimmedHistory :+ state, true, openBracketsCnt)
        else if (!foundOpener && state == openState && openBracketsCnt == 1) (trimmedHistory :+ state, true, 0)
        else if (!foundOpener && state == openState && openBracketsCnt > 0) (trimmedHistory, false, openBracketsCnt - 1)
        else if (!foundOpener && state == closeState) (trimmedHistory, false, openBracketsCnt + 1)
        else aggregate
      })._1

    val trimmedHistory =
      if (history.nonEmpty && history.head == ReadObjectEnd) trim2(ReadObject, ReadObjectEnd)
      else if (history.nonEmpty && history.head == ReadArrayEnd) trim2(ReadArray, ReadArrayEnd)
      else history

    if (trimmedHistory.size >= 2) trimmedHistory.tail.head == ReadObjectKey
    else false
  }

  private def createParsingTuple(sm: StateMachine,
                                 aggregate: ParsingTuple,
                                 history: Seq[State],
                                 actionTuple: (Navigation, NodeKind, Option[CreatorArgument => JValue])) = {
    val (navigation, childKind, f) = actionTuple
    val newTree = addChild(aggregate, childKind, f)
    val rightmostPath = whenNodeAddedTakeRightmostChild(aggregate, childKind, newTree)
    val currentTreePath =
      if (navigation == Navigation.Up2) rightmostPath.init.init
      else if (navigation == Navigation.Up) rightmostPath.init
      else if (navigation == Navigation.Stay) updatePathObjects(aggregate.currentTreePath, rightmostPath)
      else rightmostPath

    ParsingTuple(sm, newTree, currentTreePath, aggregate.builder, history)
  }

  private def addChild(aggregate: ParsingTuple, childKind: NodeKind, f: Option[CreatorArgument => JValue]) =
    f.map(aggregate.tree.addChild(childKind, _, aggregate.currentTreePath)).getOrElse(aggregate.tree)

  private def whenNodeAddedTakeRightmostChild(aggregate: ParsingTuple, childKind: NodeKind, newTree: FunctionsTree) =
    if (childKind != NoneNode) newTree.getRightmostChildPath() else aggregate.currentTreePath

  private def updatePathObjects(currentTreePath: Seq[Node], newObjects: Seq[Node]) =
    currentTreePath
      .foldLeft((List.empty[Node], newObjects))((pair, _) => pair match {
        case (newPath, updater) => (newPath :+ updater.head, updater.tail)
      })
      ._1

  private def getCleanedPropertyKey(sb: StringBuilder) = {
    val value = sb.toString
    val firstQuoteIdx = value.indexOf('"')
    val secondQuoteIdx = value.length - value.reverse.indexOf('"')

    value.substring(firstQuoteIdx + 1, secondQuoteIdx - 1)
  }

  private def getCleanedScalar(sb: StringBuilder) = {
    val value = sb.toString.trim
    val step1 =
      if (value.last == ',') value.init.trim
      else value.trim
    val firstQuoteIdx = step1.indexOf('"')
    val secondQuoteIdx = step1.length - step1.reverse.indexOf('"')
    val step2 =
      if (firstQuoteIdx < 0) step1.trim
      else step1.substring(firstQuoteIdx + 1, secondQuoteIdx - 1)
    val step3 =
      if (step2(0) == ',') step2.substring(1).trim
      else step2

    step3
  }

  private val getPropertyKey = (name: String) =>
    (arg: CreatorArgument) => JField(name, arg.fold(CreatorArgumentToJValue))

  private val getScalar = (scalar: String) =>
    (_: CreatorArgument) => scalar.asJValue

  private val getObject = (arg: CreatorArgument) =>
    arg.select[JObject] match {
      case None => JNull
      case Some(obj) => obj
    }

  private val getArray = (arg: CreatorArgument) =>
    arg.select[JArray] match {
      case None => JNull
      case Some(array) => array
    }

  object CreatorArgumentToJValue extends Poly1 {
    implicit val atJString: Case.Aux[JString, JValue] = at { x: JString => x }
    implicit val atJInt: Case.Aux[JNumber, JValue] = at { x: JNumber => x }
    implicit val atJBool: Case.Aux[JBool, JValue] = at { x: JBool => x }
    implicit val atFields: Case.Aux[JObject, JValue] = at { x: JObject => x }
    implicit val atValues: Case.Aux[JArray, JValue] = at { x: JArray => x }
    implicit val atUnit: Case.Aux[Unit, JValue] = at { _: Unit => JNull }
  }

  private case class ParsingTuple(stateMachine: StateMachine,
                                  tree: FunctionsTree,
                                  currentTreePath: Seq[Node],
                                  builder: StringBuilder,
                                  statesHistory: Seq[State])

}
