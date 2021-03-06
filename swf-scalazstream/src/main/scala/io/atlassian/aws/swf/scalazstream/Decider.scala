package io.atlassian.aws
package swf
package scalazstream

import java.util.concurrent.ExecutorService

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.Attempt
import kadai.log.json.JsonLogging

import scalaz.concurrent.Task
import scalaz.stream.{ Process, sink, Sink }
import scalaz.syntax.monad._

class Decider(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity, executor: ExecutorService, taskList: Option[TaskList] = None) extends JsonLogging {
  import SWFAction._
  import JsonLogging._

  implicit val es = executor

  private def deciderStream: Process[Task, Option[DecisionInstance]] =
    Process.repeatEval {
      Task { pollDecision(swf, workflow.domain, taskList.getOrElse(workflow.workflowConfig.defaultTaskList), identity).run }(executor) flatMap {
        _.fold(
          invalid => Task.fail(WrappedInvalidException.orUnderlying(invalid)),
          Task.now
        )
      } handle {
        case throwable =>
          error(throwable)
          None
      }
    }

  private def decisionCompletionSink: Sink[Task, Option[DecisionInstance]] =
    sink.lift {
      case None => Task.now(())
      case Some(di) => Task {
        complete(di.taskToken, "", workflow.decisionEngine(di)).run.valueOr(inv => error(inv))
      }(executor).handle {
        case throwable =>
          error(throwable)
      }
    }

  def decider: Task[Unit] =
    (deciderStream to decisionCompletionSink).run

  private def pollDecision(swf: AmazonSimpleWorkflow, domain: Domain, taskList: TaskList, identity: SWFIdentity): Attempt[Option[DecisionInstance]] =
    SWF.poll(DecisionQuery(domain, taskList, None, None, reverseOrder = true, identity)).unsafePerform(swf)

  private def complete(taskToken: TaskToken, context: String, decisions: List[Decision]): Attempt[Unit] =
    SWF.completeDecision(taskToken, context, decisions).unsafePerform(swf)
}
