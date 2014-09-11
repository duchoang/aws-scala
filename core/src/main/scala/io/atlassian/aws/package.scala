package io.atlassian

package object aws extends Types {
  type Attempt[A] = kadai.Attempt[A]
  val Attempt = kadai.Attempt
}