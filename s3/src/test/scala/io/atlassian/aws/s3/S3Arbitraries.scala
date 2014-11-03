package io.atlassian.aws.s3

import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary._
import scalaz._

trait S3Arbitraries {

  case class ObjectToStore(key: S3Key, data: Array[Byte])

  case class S3Folders(folders: List[String]) {
    def toPrefix = s"${folders.mkString("/")}/"
  }

  implicit def S3KeyArbitrary: Arbitrary[S3Key] =
    Arbitrary(Gen.uuid.map { u => Tag(u.toString) })

  implicit def ObjectToStoreArbitrary: Arbitrary[ObjectToStore] =
    Arbitrary(
      for {
        dataLength <- Gen.choose(1, 1000000)
        key <- arbitrary[S3Key]
        data <- Gen.listOfN(dataLength, arbitrary[Byte]).map(_.toArray)
      } yield ObjectToStore(key, data)
    )

  implicit def S3FoldersArbitrary: Arbitrary[S3Folders] =
    Arbitrary(
      for {
        numFolders <- Gen.choose(1, 10)
        folders <- Gen.listOfN(numFolders, Gen.uuid.map { _.toString })
      } yield S3Folders(folders)
    )

  implicit val ArbitraryRange: Arbitrary[Range] =
    Arbitrary {
      val pos = Gen.posNum[Long]
      val all = Gen.const(Range.All)
      val from = pos.map(Range.From(_))
      val to = pos.map(Range.To(_))
      val interval = Gen.zip(pos, pos).map {
        case (s, e) if s < e => Range.Interval(s, e)
        case (e, s)          => Range.Interval(s, e)
      }
      Gen.choose(0, 3).flatMap {
        _ match {
          case 0 => all
          case 1 => from
          case 2 => to
          case 3 => interval
        }
      }
    }

  implicit class OrderPlusSyntax[F](self: F)(implicit ord: Order[F]) {
    def sort(other: F): (F, F) =
      if (ord.lessThanOrEqual(self, other))
        self -> other
      else other -> self
  }

  sealed trait LargeMarker
  type LargeObjectToStore = ObjectToStore @@ LargeMarker
  object LargeObjectToStore {
    def apply(key: S3Key, data: Array[Byte]): LargeObjectToStore =
      Tag(ObjectToStore(key, data))
  }

  implicit val LargeObjectToStoreArbitrary: Arbitrary[LargeObjectToStore] =
    Arbitrary(
      for {
        dataLength <- Gen.choose(5 * 1024 * 1024 + 1, 11 * 1024 * 1024)
        key <- arbitrary[S3Key]
        data <- Gen.listOfN(dataLength, arbitrary[Byte]).map(_.toArray)
      } yield LargeObjectToStore(key, data))

}
