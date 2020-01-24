package com.mmalek.jsonSql.execution.runnables

import com.mmalek.jsonSql.execution.extensions.JValueOps._
import com.mmalek.jsonSql.execution.runnables.Types.RunnableArgument
import com.mmalek.jsonSql.jsonParsing.dataStructures.{JDouble, JInt, JValue}
import com.mmalek.jsonSql.sqlParsing.Token.{Constant, Field}
import com.mmalek.jsonSql.types.{MaybeThat, MaybeThis, Or}
import shapeless.{Coproduct, Poly1}

class AvgFunction extends Runnable {
  def canRun(symbol: String, args: Seq[RunnableArgument]): Boolean =
    symbol == "avg" && args.length == 1 && args.head.fold(RunnableArgumentToBool)

  def run(symbol: String, args: Seq[RunnableArgument], json: JValue): Option[RunnableArgument] =
    args.head
      .fold(RunnableArgumentToValueOption)
      .map {
        case MaybeThis(fieldName) => json.getValuesFor(fieldName.split("."))
        case MaybeThat(values) => values
      }
      .flatMap(values => {
        if(hasInvalidValues(values)) None
        else {
          val numbers = values.flatten.map {
            case JInt(v) => v.asInstanceOf[Double]
            case JDouble(v) => v
            case _ => 0
          }
          val avg = numbers.sum / numbers.length

          Some(Coproduct[RunnableArgument](avg))
        }
      })

  private def hasInvalidValues(values: Seq[Option[JValue]]) =
    values.exists {
      case Some(JInt(_)) | Some(JDouble(_)) | None => false
      case _ => true
    }

  object RunnableArgumentToBool extends Poly1 {
    implicit val atField: Case.Aux[Field, Boolean] = at { _: Field => true }
    implicit val atSeq: Case.Aux[Seq[Option[JValue]], Boolean] = at { _: Seq[Option[JValue]] => true }
    implicit val atConst: Case.Aux[Constant, Boolean] = at { _: Constant => false }
    implicit val atDouble: Case.Aux[Double, Boolean] = at { _: Double => false }
    implicit val atBigInt: Case.Aux[BigInt, Boolean] = at { _: BigInt => false }
  }

  object RunnableArgumentToValueOption extends Poly1 {
    implicit val atField: Case.Aux[Field, Option[Or[String, Seq[Option[JValue]]]]] =
      at { x: Field => Some(MaybeThis(x.value)) }
    implicit val atSeq: Case.Aux[Seq[Option[JValue]], Option[Or[String, Seq[Option[JValue]]]]] =
      at { x: Seq[Option[JValue]] => Some(MaybeThat(x)) }
    implicit val atConst: Case.Aux[Constant, Option[Or[String, Seq[Option[JValue]]]]] = at { _: Constant => None }
    implicit val atDouble: Case.Aux[Double, Option[Or[String, Seq[Option[JValue]]]]] = at { _: Double => None }
    implicit val atBigInt: Case.Aux[BigInt, Option[Or[String, Seq[Option[JValue]]]]] = at { _: BigInt => None }
  }
}
