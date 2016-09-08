package io.atlassian.aws
package sns

import scalaz.@@

trait Types {
  type TopicARN = String @@ TopicARN.Marker
  object TopicARN extends Tagger[String]

  type ToSubject[A] = A => Option[String]
  type ToMessage[A] = A => String

  type MessageId = String @@ MessageId.Marker
  object MessageId extends Tagger[String]
}
