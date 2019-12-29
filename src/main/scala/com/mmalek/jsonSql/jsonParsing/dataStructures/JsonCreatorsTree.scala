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
        val values = elements.map(e => value(e.jsonTree match {
          case x: JString => Coproduct[CreatorArgument](x)
          case x: JDouble => Coproduct[CreatorArgument](x)
          case x: JInt => Coproduct[CreatorArgument](x)
          case x: JBool => Coproduct[CreatorArgument](x)
          case x: JObject => Coproduct[CreatorArgument](x.obj)
          case x: JArray => Coproduct[CreatorArgument](x.arr)
          case _ => Coproduct[CreatorArgument](())
        }))

        JNull
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
  def jsonTree: JValue = JNothing(0)
}


//package com.mmalek.jsonSql.jsonParsing.dataStructures
//
//import com.mmalek.jsonSql.jsonParsing.Types.CreatorArgument
//
//sealed trait JsonCreatorsTree {
//  def addChild(childValue: CreatorArgument => JValue, childUnderlyingValue: Any, parentPath: Seq[Node] = Nil): Node
//  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node]
//}
//
//object JsonCreatorsTree {
//  def zero: JsonCreatorsTree = Zero()
//}
//
//case class Node(value: CreatorArgument => JValue, underlyingValue: Any, children: Seq[Node]) extends JsonCreatorsTree {
//  def addChild(childValue: CreatorArgument => JValue,
//               childUnderlyingValue: Any,
//               parentPath: Seq[Node] = Nil): Node = parentPath match {
//    case Nil => createTree(childValue, childUnderlyingValue)
//    case parents => recreateTree(childValue, childUnderlyingValue, parents)
//  }
//
//  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] =
//    children match {
//      case Nil => parents :+ this
//      case elements => elements.last.getRightmostChildPath(parents :+ this)
//    }
//
//  override def toString: String =
//    s"$underlyingValue -> ${children.map(_.toString)}"
//
//  private def createTree(childValue: CreatorArgument => JValue, childUnderlyingValue: Any) =
//    copy(children = children :+ Node(childValue, childUnderlyingValue, Nil))
//
//  private def recreateTree(childValue: CreatorArgument => JValue, childUnderlyingValue: Any, parents: Seq[Node]) = {
//    val reversedParents = parents.reverse
//    val immediateParent = reversedParents.head
//    val newChildren = immediateParent.children :+ Node(childValue, childUnderlyingValue, Nil)
//    val newImmediateParent = immediateParent.copy(children = newChildren)
//    val newParents = newImmediateParent +: reversedParents.tail
//
//    newParents.reduceLeft((child, parent) => {
//      val previous = parent.children.dropRight(1)
//
//      parent.copy(children = previous :+ child)
//    })
//  }
//}
//
//case class Zero() extends JsonCreatorsTree {
//  def addChild(childValue: CreatorArgument => JValue,
//               childUnderlyingValue: Any,
//               parentPath: Seq[Node] = Nil): Node =
//    Node(childValue, childUnderlyingValue, Nil)
//
//  def getRightmostChildPath(parents: Seq[Node] = Nil): Seq[Node] = Nil
//}
