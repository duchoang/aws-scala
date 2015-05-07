package io.atlassian.aws.swf
package scalazstream

import java.util.concurrent.ExecutorService

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import io.atlassian.aws.WrappedInvalidException
import kadai.Attempt
import kadai.log.json.JsonLogging

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.syntax.monad._
import scalaz.{ -\/, \/-, Monad }

class Decider(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity, executor: ExecutorService) extends JsonLogging {
  import JsonLogging._

  implicit val es = executor

  def deciderStream: Process[Task, Option[DecisionInstance]] =
    Process.repeatEval {
      Task { pollDecision(swf, workflow, identity).run }(executor) flatMap {
        case -\/(invalid) => Task.fail(WrappedInvalidException.orUnderlying(invalid))
        case \/-(oDi)     => Task.now(oDi)
      } handle {
        case throwable =>
          error(throwable)
          None
      }
    }

  def decisionCompletionSink: Sink[Task, Option[DecisionInstance]] =
    sink.lift {
      case None     => Task.now(())
      case Some(di) => Task {
        complete(di.taskToken, "", workflow.decisionEngine(di)).run.valueOr(inv => error(inv))
      }(executor).handle {
        case throwable =>
          error(throwable)
      }
    }

  def decider: Task[Unit] =
    (deciderStream to decisionCompletionSink).run

  private def pollDecision(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity): Attempt[Option[DecisionInstance]] =
    SWF.poll(DecisionQuery(workflow.domain, workflow.workflowConfig.defaultTaskList, None, Some(25), reverseOrder = true, identity)).run(swf)

  private def complete(taskToken: TaskToken, context: String, decisions: List[Decision]): Attempt[Unit] =
    SWF.completeDecision(taskToken, context, decisions).run(swf)
}
