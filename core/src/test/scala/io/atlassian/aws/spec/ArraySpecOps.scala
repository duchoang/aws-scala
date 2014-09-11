package io.atlassian.aws.spec

import java.io.InputStream

import scalaz.syntax.id._

object ArraySpecOps {
  def toByteArray(is: InputStream): Array[Byte] =
    Array.canBuildFrom[Byte].apply() |> {
      build =>
        def loop(b: Int = is.read): Array[Byte] =
          if (b == -1) build.result
          else {
            build += b.toByte
            loop()
          }
        loop()
    }

  def matchByteContent(expected: Array[Byte]) = new NiceArrayMatcher(expected)
}
