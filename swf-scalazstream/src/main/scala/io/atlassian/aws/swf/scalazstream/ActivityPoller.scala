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
import scalaz.stream.{sink, Sink, time, Process}

import io.atlassian.aws.swf.{Result => SWFResult}

class ActivityPoller(swf: AmazonSimpleWorkflow,
                     domain: Domain,
                     identity: SWFIdentity,
                     taskList: TaskList,
                     activities: List[ActivityDefinition[Task]],
                     executorService: ExecutorService,
                     scheduledExecutorService: ScheduledExecutorService,
                     activityExecutionTimeout: FiniteDuration) extends JsonLogging {
  import JsonLogging._

  implicit val es = executorService

  lazy val activityMap = activities.map { ad => ad.activity -> ad }.toMap

  val strategy = Strategy.Executor(es)

  private def heartbeat(interval: FiniteDuration, taskToken: TaskToken): Task[Unit] =
    time.awakeEvery(interval)(strategy, scheduledExecutorService).flatMap[Task, Unit] { d =>
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

  def pollingStream: Process[Task, Option[(ActivityInstance, ActivityDefinition[Task])]] =
    Process.repeatEval {
       val t = Task {
        pollActivity.run
      }(executorService) flatMap {
        res => res fold (
          i =>
            Task.fail(WrappedInvalidException.apply(i)),
          oAi =>
            Task.now(oAi.flatMap (ai => activityMap.get(ai.activity).strengthL(ai)))
          )
      }
      t.handle {
        case thrown =>
          error(thrown)
          None
       }
    }

  def executeActivity(ai: ActivityInstance, ad: ActivityDefinition[Task]): Task[(ActivityInstance, SWFResult)] = {
    val cancel = new AtomicBoolean(false)
    heartbeat(heartbeatDuration(ad.definition), ai.taskToken).runAsyncInterruptibly({
      case -\/(t) => error(t)
      case \/-(_) => ()
    }, cancel)

    val task = Task {
      ad.function(ai).run
    }(es)
    task.timed(activityExecutionTimeout)(scheduledExecutorService).onFinish {
      _ => Task.delay { cancel.set(true) }
    }.handle {
      case t =>
        error(t)
        Result.failed("Activity execution failed", t.toString)
    }.strengthL(ai)
  }

  private def executionStream: Process[Task, Option[(ActivityInstance, SWFResult)]] =
    pollingStream flatMap {
      case None => Process.emit(None)
      case Some((ai, ad)) => Process.await(executeActivity(ai, ad))(a => Process.emit(Some(a)))
    }

  private def activityCompletionSink: Sink[Task, Option[(ActivityInstance, SWFResult)]] =
    sink.lift[Task, Option[(ActivityInstance, SWFResult)]] {
      case None => Task.now(())
      case Some((ai, swfResult)) => Task {
        swfResult.fold(fail(ai), complete(ai))
      }(executorService)
    }

  private def activityPollers: Process[Task, Process[Task, Unit]] =
    Process.repeatEval {
      Task.now(executionStream.to(activityCompletionSink))
    }

  def poller(maxConcurrentActivityExecutions: Int): Task[Unit] =
    scalaz.stream.merge.mergeN(maxConcurrentActivityExecutions)(activityPollers).run
}
