package io.atlassian.aws.swf
package scalazstream

import java.util.concurrent.ExecutorService

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.Attempt
import kadai.log.json.JsonLogging

import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.syntax.monad._
import scalaz.{ -\/, \/-, Monad }

class Decider(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity, executor: ExecutorService) extends JsonLogging {
  import JsonLogging._

  implicit val es = executor

  def decider[F[_]: Monad]: F[() => Unit] =
    Process.repeatEval {
      Task {
        pollDecision(swf, workflow, identity).flatMap {
          case Some(decisionInstance) => complete(decisionInstance.taskToken, "", workflow.decisionEngine(decisionInstance))
          case None                   => Attempt.ok(())
        }.run valueOr (error(_))
      }(executor)
    }.run.runAsyncInterruptibly {
      case -\/(t) => error(t)
      case \/-(_) => ()
    }.point[F]

  private def pollDecision(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity): Attempt[Option[DecisionInstance]] =
    SWF.poll(DecisionQuery(workflow.domain, workflow.workflowConfig.defaultTaskList, None, Some(25), reverseOrder = true, identity)).run(swf)

  private def complete(taskToken: TaskToken, context: String, decisions: List[Decision]): Attempt[Unit] =
    SWF.completeDecision(taskToken, context, decisions).run(swf)
}
