package io.atlassian.aws

import com.amazonaws.handlers.RequestHandler2
import com.amazonaws.{ AmazonServiceException, Request, Response }

import java.util.{ Map => JMap }

import scala.collection.concurrent.TrieMap
import scalaz.{ \/, Monoid }
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scala.collection.convert.decorateAsScala._

case class HttpHeaders(headers: Map[String, String])

object AWSRequestIdRetriever {
  import AwsAction._

  private val contexts = TrieMap[Int, AmazonServiceException \/ JMap[String, String]]()

  val requestHandler = new RequestHandler2 {
    override def afterError(request: Request[_], response: Response[_], e: Exception) = {
      e match {
        case ase: AmazonServiceException => contexts.put(e.hashCode, ase.left)
      }
      ()
    }

    override def afterResponse(request: Request[_], response: Response[_]) = {
      contexts.put(response.getAwsResponse.hashCode, response.getHttpResponse.getHeaders.right)
      ()
    }

    override def beforeRequest(request: Request[_]) = ()
  }

  private val httpHeaders: JMap[String, String] => HttpHeaders =
    res => HttpHeaders(res.asScala.toMap)

  private def buildMetaData[W](key: Int)(fromHeaders: Option[HttpHeaders => Option[W]])(implicit M: Monoid[W]): W =
    ~(for {
      v <- contexts.remove(key)
      hs <- v.toOption
      f <- fromHeaders
      w <- f(httpHeaders(hs))
    } yield w)

  private def buildMetaDataFromException[W](key: Int)(fromException: Option[AmazonServiceException => Option[W]])(implicit M: Monoid[W]): W =
    ~(for {
      v <- contexts.remove(key)
      ase <- v.swap.toOption
      f <- fromException
      w <- f(ase)
    } yield w)

  // A must be whatever the aws client returns
  def withClient[C, W, A](f: C => A)(fromHeaders: Option[HttpHeaders => Option[W]], fromException: Option[AmazonServiceException => Option[W]])(implicit M: Monoid[W]): AwsAction[C, W, A] =
    AwsAction.withMetaData { client: C =>
      try {
        val a = f(client)
        (buildMetaData(a.hashCode)(fromHeaders), Attempt.ok(a))
      } catch {
        case util.control.NonFatal(t) =>
          (buildMetaDataFromException(t.hashCode)(fromException), Attempt.exception(t))
      }
    } recover {
      AmazonExceptions.transformException andThen invalid[C, W, A]
    }
}
