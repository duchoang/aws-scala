package io.atlassian.aws
package sqs

import spec.ScalaCheckSpec
import org.junit.runner.RunWith

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class FromStringSpec extends ScalaCheckSpec {
  import FromString._

  def is = s2"""
    FromString should
      not fall over if it fails to decode int       $intDecodeHandlesExceptions
      not fall over if it fails to decode DateTime  $dateTimeDecodeHandlesExceptions
  """

  def intDecodeHandlesExceptions =
    IntFromString(Some("Foo")).toOr.toEither must beLeft

  def dateTimeDecodeHandlesExceptions =
    DateTimeFromString(Some("Foo")).toOr.toEither must beLeft
}
