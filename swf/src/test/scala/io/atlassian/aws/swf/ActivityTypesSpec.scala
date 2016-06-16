package io.atlassian.aws.swf

import org.specs2.mock.Mockito
import org.specs2.{ ScalaCheck, SpecificationWithJUnit }

class ActivutyTypesSpec extends SpecificationWithJUnit with ScalaCheck with Mockito {

  def is =
    s2"""
  Result fold should
    run the fail function if the result is FailedActivity           ${Result.failed("r", "d").fold(f, s, e) === "fail r d"}
    run the success function if the result is SuccessfulActivity    ${Result.success("OK").fold(f, s, e) === "success OK"}
    run the empty function if the result is Empty                   ${Result.empty.fold(f, s, e) === "empty"}
  """

  private def f(reason: String, detail: String) = s"fail $reason $detail"

  private def s(output: String) = s"success $output"

  private def e = "empty"
}