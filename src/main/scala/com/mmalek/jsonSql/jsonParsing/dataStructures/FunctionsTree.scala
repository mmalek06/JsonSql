package com.mmalek.jsonSql.jsonParsing.dataStructures

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.NodeKind.ArrayNode
import shapeless.Coproduct

import scala.annotation.tailrec

sealed trait FunctionsTree {
  def addChild(childKind: NodeKind,
               childValue: CreatorArgument => JValue,
               parentPath: Seq[Node]): Node
  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node]
  def execute: JValue
}

object FunctionsTree {
  def zero: FunctionsTree = Zero
}

case class Node(kind: NodeKind, value: CreatorArgument => JValue, children: Seq[Node]) extends FunctionsTree {
  def addChild(childKind: NodeKind,
               childValue: CreatorArgument => JValue,
               parentPath: Seq[Node]): Node = parentPath match {
    case Nil => createTree(childKind, childValue)
    case parents => recreateTree(childKind, childValue, parents)
  }

  @tailrec
  final def getRightmostChildPath(parents: Seq[Node]): Seq[Node] =
    children match {
      case Nil => parents :+ this
      case elements => elements.last.getRightmostChildPath(parents :+ this)
    }

  // TODO: should be tailrec - will revisit in the future
  final def execute: JValue =
    children match {
      case Nil => value(Coproduct[CreatorArgument](()))
      case elements =>
        val values = elements.map(_.execute)
        val argument = getArgument(values)
        val result = value(argument)

        result
    }

  override def toString: String =
    s"$kind -> ${children.map(_.toString)}"

  private def getArgument(values: Seq[JValue]) =
    values match {
      case JNull :: Nil => Coproduct[CreatorArgument](())
      case (x: JString) :: Nil => Coproduct[CreatorArgument](x)
      case (x: JNumber) :: Nil => Coproduct[CreatorArgument](x)
      case (x: JBool) :: Nil => Coproduct[CreatorArgument](x)
      case (x: JObject) :: Nil => getArgumentOnObject(x)
      case (x: JArray) :: Nil => getArgumentOnArray(x)
      case (_: JField) :: _ => Coproduct[CreatorArgument](JObject(values.asInstanceOf[Seq[JField]]))
      case (_: JValue) :: _ => Coproduct[CreatorArgument](JArray(values))
    }

  private def getArgumentOnObject(x: JObject) =
    kind match {
      case ArrayNode => Coproduct[CreatorArgument](JArray(Seq(x)))
      case _ => Coproduct[CreatorArgument](x)
    }

  private def getArgumentOnArray(x: JArray) =
    kind match {
      case ArrayNode => Coproduct[CreatorArgument](JArray(Seq(x)))
      case _ => Coproduct[CreatorArgument](x)
    }

  private def createTree(childKind: NodeKind, childValue: CreatorArgument => JValue) =
    copy(children = children :+ Node(childKind, childValue, Nil))

  private def recreateTree(childKind: NodeKind, childValue: CreatorArgument => JValue, parents: Seq[Node]) = {
    val reversedParents = parents.reverse
    val immediateParent = reversedParents.head
    val newChildren = immediateParent.children :+ Node(childKind, childValue, Nil)
    val newImmediateParent = immediateParent.copy(children = newChildren)
    val newParents = newImmediateParent +: reversedParents.tail

    newParents.reduceLeft((child, parent) => {
      val previous = parent.children.dropRight(1)

      parent.copy(children = previous :+ child)
    })
  }
}

object Zero extends FunctionsTree {
  def addChild(childKind: NodeKind,
               childValue: CreatorArgument => JValue,
               parentPath: Seq[Node] = Nil): Node = Node(childKind, childValue, Nil)
  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] = Nil
  def execute: JValue = JNothing
}
