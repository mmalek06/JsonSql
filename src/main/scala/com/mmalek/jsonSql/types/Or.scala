package com.mmalek.jsonSql.types

sealed abstract class Or[T, U]

case class MaybeThis[T, U](value: T) extends Or[T, U]
case class MaybeThat[T, U](value: U) extends Or[T, U]
