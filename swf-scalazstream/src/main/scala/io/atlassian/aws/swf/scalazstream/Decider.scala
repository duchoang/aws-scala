package io.atlassian.aws.swf
package scalazstream

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.Attempt
import kadai.log.json.JsonLogging

import scalaz.{ Monad, \/-, -\/ }
import scalaz.syntax.monad._
import scalaz.concurrent.Task
import scalaz.stream.Process

class Decider(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity) extends JsonLogging {
  import JsonLogging._
  def decider[F[_]: Monad]: F[() => Unit] =
    Process.repeatEval {
      Task.fork {
        Task.delay {
          pollDecision(swf, workflow, identity).flatMap { od =>
            od.fold(Attempt.ok(())) { d =>
              complete(d.taskToken, "", workflow.decisionEngine(d))
            }
          }.run.fold(
            { i => error(i) },
            { _ => () }
          )
        }
      }
    }.run.runAsyncInterruptibly {
      case -\/(t) => error(t)
      case \/-(_) => ()
    }.point[F]

  private def pollDecision(swf: AmazonSimpleWorkflow, workflow: WorkflowDefinition, identity: SWFIdentity) =
    SWF.poll(DecisionQuery(workflow.domain, workflow.workflowConfig.defaultTaskList, identity)).run(swf)

  private def complete(taskToken: TaskToken, context: String, decisions: List[Decision]) =
    SWF.completeDecision(taskToken, context, decisions).run(swf)
}
