package io.atlassian.aws
package s3

import com.amazonaws.services.s3.{ AmazonS3Client => SDKS3Client }
import kadai.Attempt

object S3Action {
  def value[A](v: => A): S3Action[A] =
    S3Action(_ => Attempt.ok(v))

  def config: S3Action[SDKS3Client] =
    S3Action(Attempt.ok)

  def ok[A](strict: A): S3Action[A] =
    value(strict)

  def withClient[A](f: SDKS3Client => A): S3Action[A] =
    config.map(f)

  def fail[A](msg: String): S3Action[A] =
    S3Action(_ => Attempt.fail(msg))

  def fail[A](t: Throwable): S3Action[A] =
    S3Action(_ => Attempt.exception(t))

  def apply[A](run: SDKS3Client => Attempt[A]): S3Action[A] =
    AwsAction.apply[SDKS3Client, A](run)
}
