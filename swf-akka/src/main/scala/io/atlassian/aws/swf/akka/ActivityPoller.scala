package io.atlassian.aws
package swf
package akka

import _root_.akka.actor.{ Actor, PoisonPill, Props }
import _root_.akka.util.Timeout
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.log.json.JsonLogging

import scala.concurrent.duration._
import scalaz.concurrent.Task

case class ActivityPollerConfig(pollDelay: FiniteDuration = 2.seconds,
                                masterTimeout: FiniteDuration = 5.seconds,
                                domain: Domain,
                                identity: SWFIdentity)

object ActivityPoller {
  def props(swf: AmazonSimpleWorkflow, config: ActivityPollerConfig, taskList: TaskList, activities: List[ActivityDefinition[Task]]): Props =
    Props(classOf[ActivityPoller], swf, config, taskList, activities)

  object Protocol {
    case object Poll
  }
}

class ActivityPoller(swf: AmazonSimpleWorkflow,
                     config: ActivityPollerConfig,
                     taskList: TaskList,
                     activities: List[ActivityDefinition[Task]]) extends Actor with JsonLogging {

  import context.dispatcher
  import ActivityPoller.Protocol._
  import SWFAction._
  import kadai.Invalid.syntax._
  import kadai.log.json.JsonLogging._
  implicit val timeout = Timeout(config.masterTimeout)

  lazy val activityMap = activities.map { ad => ad.activity -> ad }.toMap

  def receive = {
    case PoisonPill =>
      context.stop(self)
    case Poll =>
      SWF.poll(ActivityQuery(taskList = taskList, identity = config.identity, domain = config.domain)).unsafePerform(swf).fold(
        { i => warn(i); triggerPoll },
        {
          case None => triggerPoll
          case Some(ai) =>
            val ad = activityMap(ai.activity)
            val newActor = context.actorOf(ActivityActor.props(swf, ad.definition, ai, ad.function))

            newActor ! ActivityActor.Protocol.Start
            triggerPoll
        }
      )
      ()
    case m =>
      warn(s"Unknown message $m, expect only Poll or PoisonPill".invalid)
      triggerPoll
      ()
  }

  private def triggerPoll =
    context.system.scheduler.scheduleOnce(config.pollDelay, self, Poll)
}
