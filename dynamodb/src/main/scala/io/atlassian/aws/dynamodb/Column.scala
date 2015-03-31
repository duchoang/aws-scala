package io.atlassian.aws.dynamodb

import Unmarshaller._
import kadai.Attempt
import scalaz.InvariantFunctor
import scalaz.syntax.id._

sealed trait Column[A] {
  self =>

  def marshall: Marshaller[A]
  def unmarshall: Unmarshaller[A]

  // invariant map
  def xmap[B](f: A => B, g: B => A): Column[B] =
    new Column[B] {
      override val marshall = self.marshall.contramap(g)
      override val unmarshall = self.unmarshall.map(f)
    }

  def liftOption =
    new Column[Option[A]] {
      override val marshall = self.marshall.liftOption
      override val unmarshall = self.unmarshall.liftOption
    }
}

/**
 * A specific field/column in a table. Has a name and an Encoder/Decoder to
 * prepare the encoded representation to the Dynamo driver, and to return
 * the de-serialized value back from the database, respectively.
 */
final class NamedColumn[A](val name: String)(implicit encoder: Encoder[A], decoder: Decoder[A]) extends Column[A] {
  override val marshall = Marshaller[A] { a => Map(name -> encoder.encode(a)) }
  override val unmarshall = Unmarshaller.get(name)
}

object Column extends ColumnComposites {
  def apply[A: Encoder: Decoder](s: String): NamedColumn[A] =
    new NamedColumn[A](s)

  private[dynamodb] def unmarshall[A, B](ca: Column[A], cb: Column[B])(map: DynamoMap): Attempt[(A, B)] =
    for {
      a <- ca.unmarshall(map)
      b <- cb.unmarshall(map)
    } yield (a, b)

  implicit object ColumnInvariantFunctor extends InvariantFunctor[Column] {
    def xmap[A, B](ca: Column[A], f: A => B, g: B => A): Column[B] =
      ca.xmap(f, g)
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
        val marshall =
          Marshaller[A] {
            from(_) |> { case (b, c) => append(cb.marshall(b), cc.marshall(c)) }
          }

        val unmarshall =
          for {
            b <- cb.unmarshall
            c <- cc.unmarshall
          } yield to(b, c)
      }
  }

  def compose3[A] = new Compose[A] {
    def apply[B, C, D](cb: Column[B], cc: Column[C], cd: Column[D])(from: A => (B, C, D))(to: (B, C, D) => A): Column[A] =
      new Column[A] {
        val marshall =
          Marshaller[A] {
            from(_) |> { case (b, c, d) => append(cb.marshall(b), cc.marshall(c), cd.marshall(d)) }
          }

        val unmarshall =
          for {
            b <- cb.unmarshall
            c <- cc.unmarshall
            d <- cd.unmarshall
          } yield to(b, c, d)
      }
  }

  def compose4[A] = new Compose[A] {
    def apply[B, C, D, E](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E])(from: A => (B, C, D, E))(to: (B, C, D, E) => A): Column[A] =
      new Column[A] {
        val marshall =
          Marshaller[A] {
            from(_) |> { case (b, c, d, e) => append(cb.marshall(b), cc.marshall(c), cd.marshall(d), ce.marshall(e)) }
          }

        val unmarshall =
          for {
            b <- cb.unmarshall
            c <- cc.unmarshall
            d <- cd.unmarshall
            e <- ce.unmarshall
          } yield to(b, c, d, e)
      }
  }

  def compose5[A] = new Compose[A] {
    def apply[B, C, D, E, F](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E], cf: Column[F])(from: A => (B, C, D, E, F))(to: (B, C, D, E, F) => A): Column[A] =
      new Column[A] {
        val marshall =
          Marshaller[A] {
            from(_) |> { case (b, c, d, e, f) => append(cb.marshall(b), cc.marshall(c), cd.marshall(d), ce.marshall(e), cf.marshall(f)) }
          }

        val unmarshall =
          for {
            b <- cb.unmarshall
            c <- cc.unmarshall
            d <- cd.unmarshall
            e <- ce.unmarshall
            f <- cf.unmarshall
          } yield to(b, c, d, e, f)
      }
  }

  def compose6[A] = new Compose[A] {
    def apply[B, C, D, E, F, G](cb: Column[B], cc: Column[C], cd: Column[D], ce: Column[E], cf: Column[F], cg: Column[G])(from: A => (B, C, D, E, F, G))(to: (B, C, D, E, F, G) => A): Column[A] =
      new Column[A] {
        val marshall =
          Marshaller[A] {
            from(_) |> {
              case (b, c, d, e, f, g) =>
                append(cb.marshall(b), cc.marshall(c), cd.marshall(d), ce.marshall(e), cf.marshall(f), cg.marshall(g))
            }
          }

        val unmarshall =
          for {
            b <- cb.unmarshall
            c <- cc.unmarshall
            d <- cd.unmarshall
            e <- ce.unmarshall
            f <- cf.unmarshall
            g <- cg.unmarshall
          } yield to(b, c, d, e, f, g)
      }
  }

  //  private def toMap[A](a: A, col: Column[A]): KeyValue =
  //    col.marshall(a)

  private def append(maps: KeyValue*): KeyValue = {
    val builder = Map.newBuilder[String, Value]
    maps.foreach { builder.++= }
    builder.result
  }
}