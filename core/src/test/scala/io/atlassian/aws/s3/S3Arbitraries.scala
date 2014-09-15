package io.atlassian.aws.s3

import org.scalacheck.{Arbitrary, Gen}
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

  implicit val ArbitraryRangeTuple: Arbitrary[Option[(Long, Long)]] =
    Arbitrary {
      val positiveLongGenerator = Gen.chooseNum(0, Long.MaxValue)
      val rangeTupleGenerator = Gen.zip(positiveLongGenerator, positiveLongGenerator)
      Gen.option(rangeTupleGenerator)
    }

  implicit class OrderPlusSyntax[F](self: F)(implicit ord: Order[F]) {
    def sort(other: F): (F, F) =
      if (ord.lessThanOrEqual(self, other))
        self -> other
      else other -> self
  }
}
