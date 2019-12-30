package com.mmalek.jsonSql.jsonParsing.dataStructures

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
import shapeless.Coproduct

sealed trait JsonCreatorsTree {
  def addChild(childKind: NodeKind,
               childValue: CreatorArgument => JValue,
               parentPath: Seq[Node] = Nil): Node
  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node]
  def jsonTree: JValue
}

object JsonCreatorsTree {
  def zero: JsonCreatorsTree = Zero()
}

case class Node(kind: NodeKind, value: CreatorArgument => JValue, children: Seq[Node]) extends JsonCreatorsTree {
  def addChild(childKind: NodeKind,
               childValue: CreatorArgument => JValue,
               parentPath: Seq[Node] = Nil): Node = parentPath match {
    case Nil => createTree(childKind, childValue)
    case parents => recreateTree(childKind, childValue, parents)
  }

  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] =
    children match {
      case Nil => parents :+ this
      case elements => elements.last.getRightmostChildPath(parents :+ this)
    }

  def jsonTree: JValue =
    children match {
      case Nil => value(Coproduct[CreatorArgument](()))
      case elements =>
        val values = elements.map(_.jsonTree)
        val argument = values match {
          case (x: JString) :: Nil => Coproduct[CreatorArgument](x)
          case (x: JDouble) :: Nil => Coproduct[CreatorArgument](x)
          case (x: JInt) :: Nil => Coproduct[CreatorArgument](x)
          case (x: JBool) :: Nil => Coproduct[CreatorArgument](x)
          case (x: JObject) :: Nil => Coproduct[CreatorArgument](x)
          case (x: JArray) :: Nil => Coproduct[CreatorArgument](x)
          case x: Seq[JField] => Coproduct[CreatorArgument](JObject(x))
          case x: Seq[JValue] => Coproduct[CreatorArgument](JArray(x))
        }
        val result = value(argument)

        result
    }

  override def toString: String =
    s"$kind -> ${children.map(_.toString)}"

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

case class Zero() extends JsonCreatorsTree {
  def addChild(childKind: NodeKind,
               childValue: CreatorArgument => JValue,
               parentPath: Seq[Node] = Nil): Node = Node(childKind, childValue, Nil)
  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] = Nil
  def jsonTree: JValue = JNothing
}
