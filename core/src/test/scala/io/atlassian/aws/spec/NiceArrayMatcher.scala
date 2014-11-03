package io.atlassian.aws
package spec

import org.specs2.matcher.{ Expectable, Matcher }

class NiceArrayMatcher[A](expected: Array[A]) extends Matcher[Array[A]] {

  private def checkContent(expected: Array[A], received: Array[A]): (Boolean, String) = {
    @annotation.tailrec
    def loop(i: Int): (Boolean, String) =
      if (i >= expected.length) (true, "Content matches")
      else {
        val expectedValue = expected(i)
        val resultValue = received(i)
        if (expectedValue != resultValue)
          (false, s"Content does not match at index $i: Expected $expectedValue but got $resultValue")
        else loop(i + 1)
      }
    loop(0)
  }

  def apply[S <: Array[A]](s: Expectable[S]) = {
    val valueToTest: Array[A] = s.value
    val (check, failMessage) =
      if (valueToTest.length == expected.length)
        checkContent(expected, valueToTest)
      else
        (false, s"Lengths should match. Expected ${expected.length}, got ${valueToTest.length}")

    result(check, "Content and lengths match", failMessage, s)
  }
}