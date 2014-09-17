package io.atlassian.aws
package sqs

import scala.concurrent.duration.Duration
import scala.collection.JavaConverters._

case class QueueParameters(name: String,
                           delay: Option[Duration] = None,
                           maxMessageSizeKb: Option[Int] = None,
                           messageRetentionPeriod: Option[Duration] = None,
                           policy: Option[String] = None,
                           receiveMessageWaitTime: Option[Duration] = None,
                           visibilityTimeout: Option[Duration] = None) {

  private [sqs] val attributes: java.util.Map[String, String] = {
    (delay.map { v => "DelaySeconds" -> v.toSeconds.toString} ::
      maxMessageSizeKb.map { v => "MaximumMessageSize" -> v.toString} ::
      messageRetentionPeriod.map { v => "MessageRetentionPeriod" -> v.toSeconds.toString} ::
      policy.map { v => "Policy" -> v.toString} ::
      receiveMessageWaitTime.map { v => "ReceiveMessageWaitTimeSeconds" -> v.toSeconds.toString} ::
      visibilityTimeout.map { v => "VisibilityTimeout" -> v.toSeconds.toString} ::
      Nil).flatten.toMap.asJava
  }
}