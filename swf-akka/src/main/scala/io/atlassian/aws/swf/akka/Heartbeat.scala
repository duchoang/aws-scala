package io.atlassian.aws.swf.akka

import akka.actor.{ Actor, Props, ActorRef }
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import io.atlassian.akka.Log
import io.atlassian.aws.swf._
import kadai.Invalid
import kadai.log.json.JsonLogging

import scala.concurrent.duration.FiniteDuration

object Heartbeat {
  def props(swf: AmazonSimpleWorkflow, owner: ActorRef, taskToken: TaskToken, interval: FiniteDuration): Props =
    Props(classOf[Heartbeat], swf, owner, taskToken, interval)

  object Protocol {
    case object Poll
    case class HeartbeatError(invalid: Invalid)
    case object Cancelled
  }
}

class Heartbeat(swf: AmazonSimpleWorkflow, owner: ActorRef, taskToken: TaskToken, interval: FiniteDuration) extends Actor with JsonLogging {
  import Heartbeat.Protocol._
  import context.dispatcher
  import JsonLogging._

  private val heartbeat =
    context.system.scheduler.schedule(interval / 2, interval, self, Poll)

  override def postStop(): Unit = {
    heartbeat.cancel()
    ()
  }

  def receive = {
    case Poll =>
      debug(Log(s"Sending heartbeat for task: $taskToken"))
      SWF.heartbeat(taskToken).run(swf).fold(
        { i => owner ! HeartbeatError(i) },
        { r => if (r.isCancelRequested) owner ! Cancelled }
      )
  }
}