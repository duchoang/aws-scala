package io.atlassian.scalaz71

import scalaz.concurrent.{ Task, Future }
import scala.concurrent.duration._
import scalaz.{ -\/, \/ }

/**
 * Bringing Scalaz 7.1 goodness for Futures and Tasks to the here and now.
 *
 * TODO - Remove this when we migrate to Scalaz 7.1
 */
object EnhancedScalazConcurrent {
  implicit class FutureWithAfter[A](baseFuture: Future[A]) {
    /**
     * Returns a `Future` that delays the execution of this `Future` by the duration `t`.
     */
    def after(t: Duration): Future[A] =
      after(t.toMillis)

    def after(t: Long): Future[A] =
      Timer.default.valueWait((), t).flatMap(_ => baseFuture)
  }
  /**
   * Add retry capability for task. Taken from latest scalaz.concurrent.Task
   *
   * TODO - Remove this when we upgrade to Scalaz 7.1
   *
   * @param baseTask The Task to enhance.
   * @tparam A Type param for the task.
   */
  implicit class TaskWithRetry[A](baseTask: Task[A]) {
    /**
     * Retries this task if it fails, once for each element in `delays`,
     * each retry delayed by the corresponding duration.
     * A retriable failure is one for which the predicate `p` returns `true`.
     */
    def retry(delays: Seq[Duration], p: (Throwable => Boolean) = _.isInstanceOf[Exception]): Task[A] =
      retryInternal(delays, p, false).map(_._1)

    private def retryInternal(delays: Seq[Duration],
                              p: (Throwable => Boolean),
                              accumulateErrors: Boolean): Task[(A, List[Throwable])] = {
      def help(ds: Seq[Duration], es: => Stream[Throwable]): Future[Throwable \/ (A, List[Throwable])] = {
        def acc = if (accumulateErrors) es.toList else Nil
        ds match {
          case Seq() => baseTask.get map (_.map(_ -> acc))
          case Seq(t, ts @ _*) => baseTask.get flatMap {
            case -\/(e) if p(e) =>
              help(ts, e #:: es) after t
            case x => Future.now(x.map(_ -> acc))
          }
        }
      }
      Task.async { help(delays, Stream()).runAsync }
    }
  }

}
