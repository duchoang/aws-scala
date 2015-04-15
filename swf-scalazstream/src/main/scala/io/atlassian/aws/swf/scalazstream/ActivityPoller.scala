package io.atlassian.aws
package swf
package scalazstream

import java.util.concurrent.{ ExecutorService, ScheduledExecutorService }
import java.util.concurrent.atomic.AtomicBoolean

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.log.json.JsonLogging

import scala.concurrent.duration._
import scalaz.{ Monad, \/-, -\/ }
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.std.option._
import scalaz.syntax.std.option._

import scalaz.concurrent.{ Strategy, Task }
import scalaz.stream.Process

class ActivityPoller(swf: AmazonSimpleWorkflow,
                     domain: Domain,
                     identity: SWFIdentity,
                     taskList: TaskList,
                     activities: List[ActivityDefinition[Task]],
                     executorService: ExecutorService,
                     scheduledExecutorService: ScheduledExecutorService) extends JsonLogging {
  import JsonLogging._

  implicit val es = executorService

  lazy val activityMap = activities.map { ad => ad.activity -> ad }.toMap

  val strategy = Strategy.Executor(es)

  private def heartbeat(interval: FiniteDuration, taskToken: TaskToken): Task[Unit] =
    Process.awakeEvery(interval)(strategy, scheduledExecutorService).flatMap[Task, Unit] { d =>
      SWF.heartbeat(taskToken).run(swf).fold(
        { i => Process.fail(WrappedInvalidException.orUnderlying(i)) },
        { _ => Process.empty }
      )
    }.run

  private def runSWFAction[A](a: SWFAction[A]): Unit =
    a.run(swf).run.fold({ i => error(i) }, { _ => () })

  private def pollActivity =
    SWF.poll(ActivityQuery(taskList = taskList, identity = identity, domain = domain)).run(swf)

  private def fail(instance: ActivityInstance)(reason: String, detail: String): Unit =
    withDebug(s"Failing activity ${instance.activity} id ${instance.id} with reason $reason:$detail") {
      runSWFAction(SWF.failActivity(instance.taskToken, reason, detail))
    }

  private def complete(instance: ActivityInstance)(result: String): Unit =
    withDebug(s"Completed activity ${instance.activity} id ${instance.id}") {
      runSWFAction(SWF.completeActivity(instance.taskToken, result))
    }

  private def heartbeatDuration(config: ActivityConfig): FiniteDuration =
    config.defaultTaskHeartbeatTimeout.fold(30.seconds) { s =>
      if (s.isFinite()) (s.toSeconds / 2.0).seconds
      else 30.seconds
    }

  def poller[F[_]: Monad]: F[() => Unit] =
    Process.repeatEval {
      for {
        unitOrActivity <- Task {
          pollActivity.fold(
            { i => warn(i); ().left },
            {
              case None     => ().left
              case Some(ai) => activityMap.get(ai.activity).strengthL(ai) \/> (())
            })
        }(executorService)
        unitOrResult <- unitOrActivity map {
          case (ai, ad) =>
            val cancel = new AtomicBoolean(false)
            heartbeat(heartbeatDuration(ad.definition), ai.taskToken).runAsyncInterruptibly({
              case -\/(t) => error(t)
              case \/-(_) => ()
            }, cancel)

            ad.function(ai).timed(10000)(scheduledExecutorService).onFinish { _ => Task.delay { cancel.set(true) } }
              .handle {
                case t =>
                  error(t)
                  Result.failed(t.getMessage, t.getMessage)
              }.map { r => (ai, r).right }
        } valueOr (_.left.point[Task])
        _ <- Task.delay { unitOrResult.map { case (ai, r) => r.fold(fail(ai), complete(ai)) } }
      } yield ()
    }.run.runAsyncInterruptibly {
      case -\/(t) => error(t)
      case \/-(_) => ()
    }.point[F]
}
