package io.atlassian.aws.swf
package activities

import argonaut._, Argonaut._
import io.atlassian.aws.swf.Decision.FailWorkflowExecution
import io.atlassian.aws.swf.WorkflowEvent.ActivityScheduled
import kadai.Invalid
import kadai.log.json.JsonLogging

import scalaz.Monad
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.monad._

object StatusCheck {

  def function[F[_]: Monad](f: Option[String] => F[StatusCheck.Response]): ActivityFunction[F] =
    { (task: ActivityInstance) =>
      f(task.input).map { r =>
        import io.atlassian.aws.swf.{ Result => SResult }
        import SResult._
        val json = r.jencode.nospaces
        r.fold(
          { i => failed(i.toString, json) },
          { s => failed(s, json) },
          { s => success(json) },
          { s => success(json) }
        )
      }
    }

  def handleStatusCheckCompleted(scheduledDetails: ActivityScheduled.Details,
                                 result: Option[String],
                                 onComplete: String => List[Decision]): List[Decision] = {
    import StatusCheck.Response._
    (for {
      resultString <- result
      nextActivities <- resultString.decodeOption[GoodStatus].map {
        case GoodStatus(s) => onComplete(s)
      }.orElse(resultString.decodeOption[InProgressStatus].map[List[Decision]] { _ =>
        LoopingActivity.scheduleNextLoop(scheduledDetails)
      })
    } yield nextActivities) | FailWorkflowExecution(s"Invalid activity completed state for ${scheduledDetails.activityId}", s"Could not decode result $result").list
  }

  sealed trait Response {
    import Response._
    def fold[X](e: Invalid => X, b: String => X, g: String => X, i: String => X): X = this match {
      case Error(in)           => e(in)
      case BadStatus(s)        => b(s)
      case GoodStatus(s)       => g(s)
      case InProgressStatus(s) => i(s)
    }
  }
  object Response {
    import JsonLogging._

    case class BadStatus(out: String) extends Response
    case class GoodStatus(out: String) extends Response
    case class InProgressStatus(out: String) extends Response
    case class Error(invalid: Invalid) extends Response

    def bad(out: String): Response =
      BadStatus(out)

    def inProgress(out: String): Response =
      InProgressStatus(out)

    def good(str: String): Response =
      GoodStatus(str)

    def error(invalid: Invalid): Response =
      Error(invalid)

    implicit val GoodStatusDecodeJson: DecodeJson[GoodStatus] =
      statusDecoder("good", GoodStatus.apply)

    implicit val InProgressStatusDecodeJson: DecodeJson[InProgressStatus] =
      statusDecoder("in-progress", InProgressStatus.apply)

    private def statusDecoder[A](statusString: String, f: String => A): DecodeJson[A] =
      DecodeJson { c =>
        for {
          status <- (c --\ "status").as[String]
          reason <- (c --\ "reason").as[String]
          _ <- (status == statusString).unlessM(DecodeResult.fail("Incorrect status", c.history))
        } yield f(reason)
      }

    implicit val ResponseEncodeJson: EncodeJson[Response] =
      EncodeJson {
        _.fold(
          { i => ("status" := "error") ->: ("reason" := i) ->: jEmptyObject },
          { s => ("status" := "bad") ->: ("reason" := s) ->: jEmptyObject },
          { s => ("status" := "good") ->: ("reason" := s) ->: jEmptyObject },
          { s => ("status" := "in-progress") ->: ("reason" := s) ->: jEmptyObject }
        )
      }
  }
}
