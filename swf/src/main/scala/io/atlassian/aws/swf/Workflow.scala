package io.atlassian.aws.swf

import com.amazonaws.services.simpleworkflow.model.WorkflowType

case class Workflow(name: String, version: String) {
  private[swf] val aws: WorkflowType =
    new WorkflowType().withVersion(version).withName(name)
}

object Workflow {
  def apply(w: WorkflowType): Workflow =
    Workflow(w.getName, w.getVersion)
}
