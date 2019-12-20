package com.mmalek.jsonSql.jsonParsing.dataStructures

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument

sealed trait JsonCreatorsTree {
  def addChild(childValue: CreatorArgument => JValue, childUnderlyingValue: Any, parentPath: Seq[Node] = Nil): Node
  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node]
}

object JsonCreatorsTree {
  def zero: JsonCreatorsTree = Zero()
}

case class Node(value: CreatorArgument => JValue, underlyingValue: Any, children: Seq[Node]) extends JsonCreatorsTree {
  def addChild(childValue: CreatorArgument => JValue,
               childUnderlyingValue: Any,
               parentPath: Seq[Node] = Nil): Node = parentPath match {
    case Nil => createTree(childValue, childUnderlyingValue)
    case parents => recreateTree(childValue, childUnderlyingValue, parents)
  }

  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] =
    children match {
      case Nil => parents :+ this
      case elements => elements.last.getRightmostChildPath(parents :+ this)
    }

  override def toString: String =
    s"$underlyingValue -> ${children.map(_.toString)}"

  private def createTree(childValue: CreatorArgument => JValue, childUnderlyingValue: Any) =
    copy(children = children :+ Node(childValue, childUnderlyingValue, Nil))

  private def recreateTree(childValue: CreatorArgument => JValue, childUnderlyingValue: Any, parents: Seq[Node]) = {
    val reversedParents = parents.reverse
    val immediateParent = reversedParents.head
    val newChildren = immediateParent.children :+ Node(childValue, childUnderlyingValue, Nil)
    val newImmediateParent = immediateParent.copy(children = newChildren)
    val newParents = newImmediateParent +: reversedParents.tail

    newParents.reduceLeft((child, parent) => {
      val previous = parent.children.dropRight(1)

      parent.copy(children = previous :+ child)
    })
  }
}

case class Zero() extends JsonCreatorsTree {
  def addChild(childValue: CreatorArgument => JValue,
               childUnderlyingValue: Any,
               parentPath: Seq[Node] = Nil): Node =
    Node(childValue, childUnderlyingValue, Nil)

  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] = Nil
}
