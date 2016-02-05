package io.atlassian.aws
package swf
package akka

import _root_.akka.actor.{ Props, PoisonPill, Actor }
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.Invalid
import kadai.log.json.JsonLogging

import scalaz.concurrent.Task
import scalaz.{ \/-, -\/, \/ }
import scala.concurrent.duration._
import scalaz.syntax.std.option._

object ActivityActor {
  def props(swf: AmazonSimpleWorkflow, activityDef: ActivityConfig, instance: ActivityInstance, function: ActivityFunction[Task]): Props =
    Props(classOf[ActivityActor], swf, activityDef, instance, function)

  object Protocol {
    case object Start
  }
}

class ActivityActor(swf: AmazonSimpleWorkflow, activityDef: ActivityConfig, instance: ActivityInstance, function: ActivityFunction[Task]) extends Actor with JsonLogging {
  import JsonLogging._
  import SWFAction._

  def receive = {
    case ActivityActor.Protocol.Start =>
      heartbeatDuration.foreach { d =>
        context.actorOf(Heartbeat.props(swf, self, instance.taskToken, d))
      }

      function(instance).runAsync {
        case -\/(t) => fail(t.getMessage, t.getMessage)
        case \/-(r) => r.fold(fail, complete, ())
      }
    case Heartbeat.Protocol.Cancelled =>
      cancelled()
  }

  lazy val heartbeatDuration: Option[FiniteDuration] =
    activityDef.defaultTaskHeartbeatTimeout.fold(30.seconds.some) { s =>
      if (s.isFinite()) (s.toSeconds / 2.0).seconds.some
      else 30.seconds.some
    }

  def fail(reason: String, detail: String): Unit =
    withDebug(s"Failing activity ${instance.activity} id ${instance.id} with reason $reason:$detail") {
      runSWFAction(SWF.failActivity(instance.taskToken, reason, detail))
      self ! PoisonPill
    }

  def complete(result: String): Unit =
    withDebug(s"Completed activity ${instance.activity} id ${instance.id}") {
      runSWFAction(SWF.completeActivity(instance.taskToken, result))
      self ! PoisonPill
    }

  def cancelled(): Unit =
    withDebug(s"Cancelled activity ${instance.activity} id ${instance.id}") {
      runSWFAction(SWF.cancelActivity(instance.taskToken))
      self ! PoisonPill
    }

  private def runSWFAction[A](a: SWFAction[A]): Invalid \/ A =
    a.runAction(swf).run
}