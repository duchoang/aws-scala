package io.atlassian.aws.swf

import scalaz.Monad

case class ActivityDefinition[F[_]](activity: Activity, definition: ActivityConfig, function: ActivityFunction[F])

trait WorkflowDefinition {
  def domain: Domain
  def domainConfig: DomainConfig
  def workflow: Workflow
  def workflowConfig: WorkflowConfig
  def activities[F[_]: Monad]: List[ActivityDefinition[F]]
  def activityTaskList: TaskList
  def decisionEngine: DecisionFunction
}
