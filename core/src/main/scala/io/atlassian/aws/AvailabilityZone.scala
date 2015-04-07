package io.atlassian.aws

sealed trait AvailabilityZone {
  import AvailabilityZone._

  def fold[X](multi: => X, zone: String => X): X =
    this match {
      case Multiple => multi
      case Zone(n)  => zone(n)
    }
}

object AvailabilityZone {
  case object Multiple extends AvailabilityZone

  // TODO - the zone should be an enum really
  case class Zone(name: String) extends AvailabilityZone
}