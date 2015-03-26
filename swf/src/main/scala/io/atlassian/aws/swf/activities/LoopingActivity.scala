package io.atlassian.aws.swf
package activities

import argonaut.Argonaut._
import argonaut.CodecJson
import Decision.{StartTimer, FailWorkflowExecution}
import WorkflowEvent.{ActivityScheduled, TimerStarted}

import scala.concurrent.duration.FiniteDuration

import scalaz.syntax.std.option._


case class LoopingActivity(activity: Activity, input: Option[String], maxRetry: Int, delay: FiniteDuration, retryCount: Int = 0)

object LoopingActivity {
  import JsonCodecs._, Workflows._

  implicit val delayedTaskParametersCodecJson: CodecJson[LoopingActivity] =
    casecodec5(LoopingActivity.apply, LoopingActivity.unapply)("activity", "input", "max-retry", "delay", "retry-count")


  def startLoopedActivity(timerInfo: TimerStarted): List[Decision] =
    (for {
      control <- timerInfo.control
      params <- control.decodeOption[LoopingActivity]
    } yield {
      scheduleActivity(params.activity, params.input, timerInfo.control)
    }) | FailWorkflowExecution("Invalid state of timer event", s"Unable to decode control for $timerInfo").list

  def scheduleNextLoop(lastScheduled: ActivityScheduled.Details): List[Decision] =
    (for {
      controlString <- lastScheduled.control
      taskParams <- controlString.decodeOption[LoopingActivity]
    } yield {
      if (taskParams.retryCount < taskParams.maxRetry)
        startLoop(taskParams.copy(retryCount = taskParams.retryCount + 1))
      else
        FailWorkflowExecution(s"Looped activity timed out", s"Activity ${taskParams.activity} with Id: ${lastScheduled.activityId}").list
    }) | FailWorkflowExecution(s"Invalid state of scheduled activity event ${lastScheduled.activityId}", s"Unable to decode control for $lastScheduled").list

  def startLoop(parameters: LoopingActivity): List[Decision] =
    StartTimer(TimerId(s"timer-${parameters.activity.name}-${java.util.UUID.randomUUID().toString}"),
      parameters.delay, parameters.jencode.nospaces.some).list
}

