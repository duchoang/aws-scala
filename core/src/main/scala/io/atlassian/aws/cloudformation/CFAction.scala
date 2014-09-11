package io.atlassian.aws
package cloudformation

import com.amazonaws.services.cloudformation.AmazonCloudFormationClient
import kadai.Attempt

object CFAction {
  def value[A](v: => A): CFAction[A] =
    CFAction(_ => Attempt.ok(v))

  def config: CFAction[AmazonCloudFormationClient] =
    CFAction(Attempt.ok)

  def ok[A](strict: A): CFAction[A] =
    value(strict)

  def withClient[A](f: AmazonCloudFormationClient => A): CFAction[A] =
    config.map(f)

  def apply[A](run: AmazonCloudFormationClient => Attempt[A]): CFAction[A] =
    AwsAction.apply[AmazonCloudFormationClient, A](run)
}
