package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.AmazonSQS

object SQSAction {
  def value[O](v: => O): SQSAction[O] =
    attempt(Attempt.ok(v))

  def config: SQSAction[AmazonSQS] =
    SQSAction { c => Attempt.ok(c) }

  def ok[O](strict: O): SQSAction[O] =
    value(strict)

  def attempt[O](attempt: Attempt[O]): SQSAction[O] =
    SQSAction { _ => attempt }

  def withClient[O](f: AmazonSQS => O): SQSAction[O] =
    config map f

  def apply[A](run: AmazonSQS => Attempt[A]): SQSAction[A] =
    AwsAction.apply[AmazonSQS, A](run)

  def fail[A](msg: String): SQSAction[A] =
    attempt(Attempt.fail(msg))
}
