package io.atlassian.aws
package swf
package akka

import _root_.akka.actor.{ Actor, Props, ActorRef }
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
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
  import AwsAction._
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
      SWF.heartbeat(taskToken).runAction(swf).fold(
        { i => owner ! HeartbeatError(i) },
        { r => if (r.isCancelRequested) owner ! Cancelled }
      )
  }
}
