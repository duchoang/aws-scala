package io.atlassian.aws
package sqs

import argonaut.Argonaut._
import argonaut.CodecJson

object Examples {
  case class Replicate(src: String, dest: String, overwrite: Boolean)

  implicit val ReplicateCodecJson: CodecJson[Replicate] =
    casecodec3(Replicate.apply, Replicate.unapply)("src", "dest", "overwrite")

  implicit val ReplicateMarshaller: Marshaller[Replicate] =
    Marshaller.jsonBody[Replicate]

  implicit val ReplicateUnmarshaller: Unmarshaller[Replicate] =
    Unmarshaller.jsonBody[Replicate]
}
