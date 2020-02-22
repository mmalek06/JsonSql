package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.sqlParsing.Token.Field
import shapeless.Poly1

object Folders {
  object IsNumeric extends Poly1 {
    implicit val atDouble: Case.Aux[BigDecimal, Boolean] = at { _: BigDecimal => true }
    implicit val atString: Case.Aux[String, Boolean] = at { _: String => false }
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => false }
    implicit val atBool: Case.Aux[Boolean, Boolean] = at { _: Boolean => false }
    implicit val atUnit: Case.Aux[Unit, Boolean] = at { _: Unit => false }
  }

  object IsString extends Poly1 {
    implicit val atString: Case.Aux[String, Boolean] = at { _: String => true }
    implicit val atDouble: Case.Aux[BigDecimal, Boolean] = at { _: BigDecimal => false }
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => false }
    implicit val atBool: Case.Aux[Boolean, Boolean] = at { _: Boolean => false }
    implicit val atUnit: Case.Aux[Unit, Boolean] = at { _: Unit => false }
  }

  object IsBoolean extends Poly1 {
    implicit val atBool: Case.Aux[Boolean, Boolean] = at { _: Boolean => true }
    implicit val atString: Case.Aux[String, Boolean] = at { _: String => false }
    implicit val atDouble: Case.Aux[BigDecimal, Boolean] = at { _: BigDecimal => false }
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => false }
    implicit val atUnit: Case.Aux[Unit, Boolean] = at { _: Unit => false }
  }

  object IsField extends Poly1 {
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => true }
    implicit val atBool: Case.Aux[Boolean, Boolean] = at { _: Boolean => false }
    implicit val atString: Case.Aux[String, Boolean] = at { _: String => false }
    implicit val atDouble: Case.Aux[BigDecimal, Boolean] = at { _: BigDecimal => false }
    implicit val atUnit: Case.Aux[Unit, Boolean] = at { _: Unit => false }
  }

  object RunnableArgumentToNumber extends Poly1 {
    implicit val atDouble: Case.Aux[BigDecimal, Option[BigDecimal]] = at { x: BigDecimal => Some(x) }
    implicit val atString: Case.Aux[String, Option[BigDecimal]] = at { _: String => None }
    implicit val atField: Case.Aux[Field, Option[BigDecimal]] = at { _: Field => None }
    implicit val atBool: Case.Aux[Boolean, Option[BigDecimal]] = at { _: Boolean => None }
    implicit val atUnit: Case.Aux[Unit, Option[Boolean]] = at { _: Unit => None }
  }

  object RunnableArgumentToString extends Poly1 {
    implicit val atString: Case.Aux[String, Option[String]] = at { x: String => Some(x) }
    implicit val atDouble: Case.Aux[BigDecimal, Option[String]] = at { _: BigDecimal => None }
    implicit val atField: Case.Aux[Field, Option[String]] = at { _: Field => None }
    implicit val atBool: Case.Aux[Boolean, Option[String]] = at { _: Boolean => None }
    implicit val atUnit: Case.Aux[Unit, Option[Boolean]] = at { _: Unit => None }
  }

  object RunnableArgumentToBoolean extends Poly1 {
    implicit val atBool: Case.Aux[Boolean, Option[Boolean]] = at { x: Boolean => Some(x) }
    implicit val atString: Case.Aux[String, Option[Boolean]] = at { _: String => None }
    implicit val atDouble: Case.Aux[BigDecimal, Option[Boolean]] = at { _: BigDecimal => None }
    implicit val atField: Case.Aux[Field, Option[Boolean]] = at { _: Field => None }
    implicit val atUnit: Case.Aux[Unit, Option[Boolean]] = at { _: Unit => None }
  }

  object RunnableArgumentToField extends Poly1 {
    implicit val atField: Case.Aux[Field, Option[Field]] = at { x: Field => Some(x) }
    implicit val atBool: Case.Aux[Boolean, Option[Field]] = at { _: Boolean => None }
    implicit val atString: Case.Aux[String, Option[Field]] = at { _: String => None }
    implicit val atDouble: Case.Aux[BigDecimal, Option[Field]] = at { _: BigDecimal => None }
    implicit val atUnit: Case.Aux[Unit, Option[Boolean]] = at { _: Unit => None }
  }

  object RunnableArgumentToValueOption extends Poly1 {
    implicit val atField: Case.Aux[Field, Option[String]] = at { x: Field => Some(x.value) }
    implicit val atDouble: Case.Aux[BigDecimal, Option[String]] = at { _: BigDecimal => None }
    implicit val atString: Case.Aux[String, Option[String]] = at { _: String => None }
    implicit val atBool: Case.Aux[Boolean, Option[String]] = at { _: Boolean => None }
    implicit val atUnit: Case.Aux[Unit, Option[Boolean]] = at { _: Unit => None }
  }
}
