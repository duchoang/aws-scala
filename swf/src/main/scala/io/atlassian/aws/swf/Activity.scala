package io.atlassian.aws.swf

import com.amazonaws.services.simpleworkflow.model.ActivityType

case class Activity(name: String, version: String) {
  val aws: ActivityType =
    new ActivityType().withName(name).withVersion(version)
}

object Activity {
  def apply(a: ActivityType): Activity =
    Activity(a.getName, a.getVersion)
}
