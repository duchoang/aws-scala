package io.atlassian.aws.dynamodb

import Marshaller._
import Unmarshaller._
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

  def liftOption =
    new Column[Option[A]] {
      val marshaller = self.marshaller.liftOption
      val unmarshaller = self.unmarshaller.liftOption
    }
}

/**
 * A specific field/column in a table. Has a name and an Encoder/Decoder to
 * prepare the encoded representation to the Dynamo driver, and to return
 * the de-serialized value back from the database, respectively.
 */
final class NamedColumn[A](val name: String)(implicit encoder: Encoder[A], decoder: Decoder[A]) extends Column[A] {
  val marshaller = Marshaller[A] { a => Map(name -> encoder.encode(a)) }
  val unmarshaller = Unmarshaller.get(name)
}

object Column extends ColumnComposites {
  def apply[A: Encoder: Decoder](s: String): NamedColumn[A] =
    new NamedColumn[A](s)

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
   * implementations define specific apply methods of the right type
   * this lets us specify the composed type and infer the rest from the passed Column types
   */
  trait Compose[To]

  def compose2[A] = new Compose[A] {
    def apply[B, C](cb: Column[B], cc: Column[C])(from: A => (B, C))(to: (B, C) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller[A] {
            from(_) |> { case (b, c) => append(toMap(b, cb), toMap(c, cc)) }
          }

        val unmarshaller =
          for {
            b <- cb.unmarshaller
            c <- cc.unmarshaller
          } yield to(b, c)
      }
  }

  def compose3[A] = new Compose[A] {
    def apply[B, C, D](cb: Column[B], cc: Column[C], cd: Column[D])(from: A => (B, C, D))(to: (B, C, D) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller[A] {
            from(_) |> { case (b, c, d) => append(toMap(b, cb), toMap(c, cc), toMap(d, cd)) }
          }

        val unmarshaller =
          for {
            b <- cb.unmarshaller
            c <- cc.unmarshaller
            d <- cd.unmarshaller
          } yield to(b, c, d)
      }
  }

  def compose4[A] = new Compose[A] {
    def apply[B, C, D, E](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E])(from: A => (B, C, D, E))(to: (B, C, D, E) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller[A] {
            from(_) |> { case (b, c, d, e) => append(toMap(b, cb), toMap(c, cc), toMap(d, cd), toMap(e, ce)) }
          }

        val unmarshaller =
          for {
            b <- cb.unmarshaller
            c <- cc.unmarshaller
            d <- cd.unmarshaller
            e <- ce.unmarshaller
          } yield to(b, c, d, e)
      }
  }

  def compose5[A] = new Compose[A] {
    def apply[B, C, D, E, F](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E], cf: Column[F])(from: A => (B, C, D, E, F))(to: (B, C, D, E, F) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller[A] {
            from(_) |> { case (b, c, d, e, f) => append(toMap(b, cb), toMap(c, cc), toMap(d, cd), toMap(e, ce), toMap(f, cf)) }
          }

        val unmarshaller =
          for {
            b <- cb.unmarshaller
            c <- cc.unmarshaller
            d <- cd.unmarshaller
            e <- ce.unmarshaller
            f <- cf.unmarshaller
          } yield to(b, c, d, e, f)
      }
  }

  def compose6[A] = new Compose[A] {
    def apply[B, C, D, E, F, G](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E], cf: Column[F], cg: Column[G])(from: A => (B, C, D, E, F, G))(to: (B, C, D, E, F, G) => A): Column[A] =
      new Column[A] {
        val marshaller =
          Marshaller[A] {
            from(_) |> {
              case (b, c, d, e, f, g) =>
                append(toMap(b, cb), toMap(c, cc), toMap(d, cd), toMap(e, ce), toMap(f, cf), toMap(g, cg))
            }
          }

        val unmarshaller =
          for {
            b <- cb.unmarshaller
            c <- cc.unmarshaller
            d <- cd.unmarshaller
            e <- ce.unmarshaller
            f <- cf.unmarshaller
            g <- cg.unmarshaller
          } yield to(b, c, d, e, f, g)
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