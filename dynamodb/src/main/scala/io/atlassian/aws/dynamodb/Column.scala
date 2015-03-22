package io.atlassian.aws.dynamodb

import Marshaller._
import Unmarshaller.Operation._
import scalaz.InvariantFunctor
import scalaz.syntax.id._

sealed trait Column[A] {
  self =>

  def marshaller: Marshaller[A]
  def unmarshaller: Unmarshaller[A]

  // invariant map
  def xmap[B](f: A => B, g: B => A): Column[B] =
    new Column[B] {
      val marshaller = self.marshaller.contramap(g)
      val unmarshaller = self.unmarshaller.map(f)
    }
}

/**
 * A specific field/column in a table. Has a name and an Encoder/Decoder to
 * prepare the encoded representation to the Dynamo driver, and to return
 * the de-serialized value back from the database, respectively.
 */
final class SingleColumn[A](val name: String)(implicit encode: Encoder[A], decode: Decoder[A]) extends Column[A] {
  def apply(a: A): Field[A] =
    set(name, a)

  val marshaller =
    new Marshaller[A] {
      def toMap(a: A): KeyValue = Map(apply(a))
    }

  val unmarshaller =
    new Unmarshaller[A] {
      def unmarshall = Unmarshaller.Operation.get(name)
    }
}

object Column extends ColumnComposites {
  def apply[A: Encoder: Decoder](s: String): Column[A] =
    new SingleColumn[A](s)

  implicit object ColumnInvariantFunctor extends InvariantFunctor[Column] {
    def xmap[A, B](ca: Column[A], f: A => B, g: B => A): Column[B] =
      ca.xmap(f, g)
  }

  /**
   * models the restrictions on types that can be used for keys
   */
  sealed trait Type
  object Type {
    case object Key
    case object Composite
  }
}

trait ColumnComposites {
  /**
   *  implementations define specific apply methods of the right type
   *  this lets us specify the composed type and infer the rest from the passed Column types
   */
  trait Compose[To]

  def compose2[A] = new Compose[A] {
    def apply[B, C](cb: Column[B], cc: Column[C])(from: A => (B, C))(to: (B, C) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller.from[A] {
            from(_) |> { case (b, c) => append(toMap(b, cb), toMap(c, cc)) }
          }

        val unmarshaller =
          Unmarshaller.from[A] {
            for {
              b <- cb.unmarshaller.unmarshall
              c <- cc.unmarshaller.unmarshall
            } yield to(b, c)
          }
      }
  }

  def compose3[A] = new Compose[A] {
    def apply[B, C, D](cb: Column[B], cc: Column[C], cd: Column[D])(from: A => (B, C, D))(to: (B, C, D) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller.from[A] {
            from(_) |> { case (b, c, d) => append(toMap(b, cb), toMap(c, cc), toMap(d, cd)) }
          }

        val unmarshaller =
          Unmarshaller.from[A] {
            for {
              b <- cb.unmarshaller.unmarshall
              c <- cc.unmarshaller.unmarshall
              d <- cd.unmarshaller.unmarshall
            } yield to(b, c, d)
          }
      }
  }

  def compose4[A] = new Compose[A] {
    def apply[B, C, D, E](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E])(from: A => (B, C, D, E))(to: (B, C, D, E) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller.from[A] {
            from(_) |> { case (b, c, d, e) => append(toMap(b, cb), toMap(c, cc), toMap(d, cd), toMap(e, ce)) }
          }

        val unmarshaller =
          Unmarshaller.from[A] {
            for {
              b <- cb.unmarshaller.unmarshall
              c <- cc.unmarshaller.unmarshall
              d <- cd.unmarshaller.unmarshall
              e <- ce.unmarshaller.unmarshall
            } yield to(b, c, d, e)
          }
      }
  }

  def compose5[A] = new Compose[A] {
    def apply[B, C, D, E, F](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E], cf: Column[F])(from: A => (B, C, D, E, F))(to: (B, C, D, E, F) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller.from[A] {
            from(_) |> { case (b, c, d, e, f) => append(toMap(b, cb), toMap(c, cc), toMap(d, cd), toMap(e, ce), toMap(f, cf)) }
          }

        val unmarshaller =
          Unmarshaller.from[A] {
            for {
              b <- cb.unmarshaller.unmarshall
              c <- cc.unmarshaller.unmarshall
              d <- cd.unmarshaller.unmarshall
              e <- ce.unmarshaller.unmarshall
              f <- cf.unmarshaller.unmarshall
            } yield to(b, c, d, e, f)
          }
      }
  }

  def compose6[A] = new Compose[A] {
    def apply[B, C, D, E, F, G](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E], cf: Column[F], cg: Column[G])(from: A => (B, C, D, E, F, G))(to: (B, C, D, E, F, G) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller.from[A] {
            from(_) |> {
              case (b, c, d, e, f, g) =>
                append(toMap(b, cb), toMap(c, cc), toMap(d, cd), toMap(e, ce), toMap(f, cf), toMap(g, cg))
            }
          }

        val unmarshaller =
          Unmarshaller.from[A] {
            for {
              b <- cb.unmarshaller.unmarshall
              c <- cc.unmarshaller.unmarshall
              d <- cd.unmarshaller.unmarshall
              e <- ce.unmarshaller.unmarshall
              f <- cf.unmarshaller.unmarshall
              g <- cg.unmarshaller.unmarshall
            } yield to(b, c, d, e, f, g)
          }
      }
  }

  private def toMap[A](a: A, col: Column[A]): KeyValue =
    col.marshaller.toMap(a)

  private def append(maps: KeyValue*): KeyValue = {
    val builder = Map.newBuilder[String, Value]
    maps.foreach { builder.++= }
    builder.result
  }
}