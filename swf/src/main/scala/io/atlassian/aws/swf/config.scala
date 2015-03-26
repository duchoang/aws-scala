package io.atlassian.aws.swf

import scala.concurrent.duration.Duration

case class DomainConfig(description: String, retentionPeriod: Duration)

case class WorkflowConfig(description: String, defaultTaskList: TaskList,
                          childPolicy: Option[ChildPolicy] = Some(ChildPolicy.Terminate), defaultTaskPriority: Int = 0,
                          defaultExecutionStartToCloseTimeout: Duration, defaultTaskStartToCloseTimeout: Option[Duration] = Some(Duration.Inf))

case class ActivityConfig(description: String, defaultTaskList: TaskList,
                          defaultTaskScheduleToStart: Option[Duration] = Some(Duration.Inf), defaultTaskScheduleToClose: Option[Duration] = Some(Duration.Inf),
                          defaultTaskPriority: Int = 0, defaultTaskHeartbeatTimeout: Option[Duration] = Some(Duration.Inf), defaultTaskStartToCloseTimeout: Option[Duration] = Some(Duration.Inf))
