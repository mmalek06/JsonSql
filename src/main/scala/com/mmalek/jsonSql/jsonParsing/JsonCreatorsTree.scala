package com.mmalek.jsonSql.jsonParsing

import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument

sealed trait JsonCreatorsTree {
  def addChild(childValue: CreatorArgument => JValue,
               parentPath: Seq[Node] = Nil): Node
}

object JsonCreatorsTree {
  def zero: JsonCreatorsTree = Zero()
}

case class Node(value: CreatorArgument => JValue, children: Seq[Node]) extends JsonCreatorsTree {
  def addChild(childValue: CreatorArgument => JValue,
               parentPath: Seq[Node] = Nil): Node = parentPath match {
    case Nil => createTree(childValue)
    case parents => recreateTree(childValue, parents)
  }

  private def createTree(childValue: CreatorArgument => JValue) =
    copy(children = children :+ Node(childValue, Nil))

  private def recreateTree(childValue: CreatorArgument => JValue, parents: Seq[Node]) = {
    val reversedParents = parents.reverse
    val immediateParent = reversedParents.head
    val newChildren = immediateParent.children :+ Node(childValue, Nil)
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
               parentPath: Seq[Node] = Nil): Node =
    Node(childValue, Nil)
}
