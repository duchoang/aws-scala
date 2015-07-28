package io.atlassian.aws

import com.amazonaws.handlers.RequestHandler2
import com.amazonaws._

import kadai.Invalid

import scalaz.Monoid
import scalaz.syntax.id._
import scalaz.syntax.monad._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scala.collection.convert.decorateAsScala._

case class HttpHeaders(headers: Map[String, String])

object AWSRequestIdRetriever {

  // [A] must be whatever the aws client returns
  def withClient[C, W, A](f: C => A)(fromHeaders: Option[HttpHeaders => Option[W]], fromException: Option[AmazonServiceException => Option[W]])(implicit M: AwsActionMonad[C, W], wmonoid: Monoid[W]): AwsAction[C, W, A] = {
    // holds the headers reference, lives for the lifetime of the call only
    class Handler extends RequestHandler2 {
      def execute(c: C): Attempt[(W, A)] = {
        val client = ConcreteClient.unapply(c)
        client.foreach { _.addRequestHandler(this) }
        try
          Attempt.safe(f(c)).map { a => (metaData, a) }
        finally
          client.foreach { _.removeRequestHandler(this) }
      }

      val tid = Thread.currentThread.getId
      var headers: Option[HttpHeaders] = none

      def metaData: W =
        ~(headers <*> fromHeaders).join

      override def afterResponse(request: Request[_], response: Response[_]): Unit =
        if (Thread.currentThread.getId == tid) // prevent multiple request threads stomping on our data
          headers = HttpHeaders(response.getHttpResponse.getHeaders.asScala.toMap).some

      override def afterError(request: Request[_], response: Response[_], e: Exception) = ()
      override def beforeRequest(request: Request[_]) = ()
    }

    def exceptionMetadata(ex: AmazonServiceException): W =
      ~{ fromException >>= { _(ex) } }

    M.ask >>= { c =>
      new Handler().execute(c).fold({
        // extract the useful info
        case Invalid.Err(ex: AmazonServiceException) =>
          M.tell(exceptionMetadata(ex)) >> M.raiseError[A](Invalid.Err(AmazonExceptions.ServiceException.from(ex).getOrElse(ex)))
        case i => M.raiseError(i)
      }, {
        case (metadata, a) => M.tell(metadata) >> M.point(a) // write the metadata
      })
    }
  }

  object ConcreteClient {
    def unapply(c: Any): Option[AmazonWebServiceClient] =
      c match {
        case client: AmazonWebServiceClient => Some(client)
        case _ => None
      }
  }
}
