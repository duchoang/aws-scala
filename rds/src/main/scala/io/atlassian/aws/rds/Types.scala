package io.atlassian.aws
package rds

import scalaz.@@

trait Types {
  type DbId = String @@ DbId.Marker
  object DbId extends Tagger[String]
}
