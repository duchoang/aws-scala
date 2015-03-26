package io.atlassian.aws
package swf

import com.amazonaws.services.simpleworkflow.model.{ PollForActivityTaskRequest, PollForDecisionTaskRequest }

case class DecisionQuery(domain: Domain, taskList: TaskList, start: Option[EventPageToken], limit: Option[Int], reverseOrder: Boolean = true, identity: SWFIdentity) {
  private[swf] val aws: PollForDecisionTaskRequest = {
    val req = new PollForDecisionTaskRequest().withDomain(domain.unwrap).withTaskList(taskList.aws)
      .withIdentity(identity.unwrap).withReverseOrder(reverseOrder)

    limit.foreach { req.setMaximumPageSize(_) }
    start.foreach { t => req.setNextPageToken(t.unwrap) }
    req
  }
}

object DecisionQuery {
  def apply(domain: Domain, taskList: TaskList, identity: SWFIdentity): DecisionQuery =
    DecisionQuery(domain, taskList, start = None, limit = None, reverseOrder = false, identity)
}

case class ActivityQuery(domain: Domain, taskList: TaskList, identity: SWFIdentity) {
  private [swf] val aws: PollForActivityTaskRequest =
    new PollForActivityTaskRequest().withDomain(domain.unwrap).withTaskList(taskList.aws).withIdentity(identity.unwrap)
}
