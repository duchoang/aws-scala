package io.atlassian.aws
package rds

import scalaz.@@

trait Types {
  sealed trait DbIdMarker
  type DbId = String @@ DbIdMarker
  object DbId extends Tagger[String, DbIdMarker]
}
