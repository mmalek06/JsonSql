package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.execution.extensions.StringOps._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.JValue
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}
import shapeless.{Coproduct, Poly1}

class AddOperator extends Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "+" && args.length == 2 && (args.forall(_.fold(IsNumeric)) || args.forall(_.fold(IsString)))

  def run(symbol: String, args: Seq[RunnableArgument], json: JValue): Option[RunnableArgument] = {
    if (args.forall(_.fold(IsNumeric))) {
      val num1 = args.head.fold(RunnableArgumentToNumber)
      val num2 = args.last.fold(RunnableArgumentToNumber)

      for {
        n1 <- num1
        n2 <- num2
      } yield Coproduct[RunnableArgument](n1 + n2)
    } else {
      val num1 = args.head.fold(RunnableArgumentToString)
      val num2 = args.last.fold(RunnableArgumentToString)

      for {
        n1 <- num1
        n2 <- num2
      } yield Coproduct[RunnableArgument](n1 + n2)
    }
  }

  object IsNumeric extends Poly1 {
    implicit val atDouble: Case.Aux[BigDecimal, Boolean] = at { _: Double => true }
    implicit val atString: Case.Aux[String, Boolean] = at { _: String => false }
    implicit val atConst: Case.Aux[Constant, Boolean] = at { x: Constant => x.value.isNumber }
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => false }
    implicit val atSeq: Case.Aux[Seq[Option[JValue]], Boolean] = at { _: Seq[Option[JValue]] => false }
  }

  object IsString extends Poly1 {
    implicit val atConst: Case.Aux[Constant, Boolean] = at { x: Constant => x.value.isString }
    implicit val atString: Case.Aux[String, Boolean] = at { _: String => true }
    implicit val atDouble: Case.Aux[BigDecimal, Boolean] = at { _: Double => false }
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => false }
    implicit val atSeq: Case.Aux[Seq[Option[JValue]], Boolean] = at { _: Seq[Option[JValue]] => false }
  }

  object RunnableArgumentToNumber extends Poly1 {
    implicit val atDouble: Case.Aux[BigDecimal, Option[BigDecimal]] = at { x: Double => Some(x) }
    implicit val atConst: Case.Aux[Constant, Option[BigDecimal]] = at { x: Constant => Some(BigDecimal(x.value)) }
    implicit val atString: Case.Aux[String, Option[BigDecimal]] = at { _: String => None }
    implicit val atField: Case.Aux[Field, Option[BigDecimal]] = at { _: Field => None }
    implicit val atSeq: Case.Aux[Seq[Option[JValue]], Option[BigDecimal]] = at { _: Seq[Option[JValue]] => None }
  }

  object RunnableArgumentToString extends Poly1 {
    implicit val atString: Case.Aux[String, Option[String]] = at { x: String => Some(x) }
    implicit val atConst: Case.Aux[Constant, Option[String]] = at { _: Constant => None }
    implicit val atDouble: Case.Aux[BigDecimal, Option[String]] = at { _: Double => None }
    implicit val atField: Case.Aux[Field, Option[String]] = at { _: Field => None }
    implicit val atSeq: Case.Aux[Seq[Option[JValue]], Option[String]] = at { _: Seq[Option[JValue]] => None }
  }
}
