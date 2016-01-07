package io.atlassian.aws.swf
package activities

import io.atlassian.aws.swf.Decision.ScheduleActivity

object Workflows {
  def scheduleActivity(activity: Activity, input: Option[String] = None, control: Option[String] = None, taskList: Option[TaskList] = None): List[Decision] =
    ScheduleActivity(activity, ActivityId(s"${activity.name}-${java.util.UUID.randomUUID().toString}"), input, control, taskList = taskList).list
}
