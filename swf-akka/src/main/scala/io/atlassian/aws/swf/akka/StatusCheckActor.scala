package io.atlassian.aws.swf.akka

import akka.actor.{Actor, ActorRef, Props}
import io.atlassian.aws.swf.activities.StatusCheck.Response

import scala.concurrent.duration.FiniteDuration

object StatusCheckActor {

  def props(owner: ActorRef, f: StatusChecker, interval: FiniteDuration): Props =
    Props(classOf[StatusCheckActor], owner, f, interval)

  object Protocol {
    private [akka] case object Poll
  }

  type StatusChecker = () => Response
  object StatusChecker {
    def apply(a: => Response): StatusChecker = () => a
  }
}

class StatusCheckActor(owner: ActorRef, f: StatusCheckActor.StatusChecker, interval: FiniteDuration) extends Actor {
  import context.dispatcher
  import io.atlassian.aws.swf.akka.StatusCheckActor.Protocol._
  private val ping = context.system.scheduler.schedule(interval / 2, interval, self, Poll)

  override def postStop(): Unit = {
    ping.cancel()
    ()
  }

  def receive = {
    case Poll =>
      owner ! f()
  }
}