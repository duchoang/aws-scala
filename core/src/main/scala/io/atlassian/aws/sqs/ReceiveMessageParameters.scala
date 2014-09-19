package io.atlassian.aws
package sqs

import scala.concurrent.duration.Duration

case class ReceiveMessageParameters(numMessages: Int = 1,
                                    visibilityTimeout: Option[Duration] = None,
                                    waitTime: Option[Duration] = None)

