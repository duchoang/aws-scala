package io.atlassian.aws

import com.amazonaws.handlers.RequestHandler2
import com.amazonaws._

import kadai.Invalid

import scalaz.Monoid
import scalaz.syntax.std.option._
import scala.collection.convert.decorateAsScala._

case class HttpHeaders(headers: Map[String, String])

object AWSRequestIdRetriever {

  private class RequestHandler[W](c: Any,
                                  fromHeaders: Option[HttpHeaders => Option[W]],
                                  fromException: Option[AmazonServiceException => Option[W]])
                                 extends RequestHandler2 {
    private type MutableHeaders = java.util.Map[String, String]
    private val client = c match {
      case client: AmazonWebServiceClient => Some(client)
      case _ => None
    }
    private val holder = new ThreadLocal[MutableHeaders]()
    override def afterError(request: Request[_], response: Response[_], e: Exception) = ()
    override def beforeRequest(request: Request[_]) = ()

    override def afterResponse(request: Request[_], response: Response[_]) = {
      holder.set(response.getHttpResponse.getHeaders)
      ()
    }

    private val httpHeaders: MutableHeaders => HttpHeaders =
      res => HttpHeaders(res.asScala.toMap)

    def addMe(): Unit = client.foreach(_.addRequestHandler(this))
    def removeMe(): Unit = client.foreach(_.removeRequestHandler(this))

    def buildMetaData(implicit M: Monoid[W]): W =
      ~(for {
        v <- Option(holder.get())
        f <- fromHeaders
        w <- f(httpHeaders(v))
      } yield w)

    def buildMetaDataFromException(ase: AmazonServiceException)(implicit M: Monoid[W]): W =
      ~(for {
        f <- fromException
        w <- f(ase)
      } yield w)
  }


  // A must be whatever the aws client returns
  def withClient[C, W, A](f: C => A)(fromHeaders: Option[HttpHeaders => Option[W]], fromException: Option[AmazonServiceException => Option[W]])(implicit monad: AwsActionMonad[C, W], wmonoid: Monoid[W]): AwsAction[C, W, A] = {
    import monad.monadSyntax._

    monad.ask >>= {
      c =>
        val handler = new RequestHandler(c, fromHeaders, fromException)
        handler.addMe()
        val result = Attempt.safe(f(c))
        handler.removeMe()

        result.fold({
          case Invalid.Err(ase: AmazonServiceException) =>
            monad.tell(handler.buildMetaDataFromException(ase)) >>= { _ => monad.raiseError[A](Invalid.Err(AmazonExceptions.ServiceException.from(ase).getOrElse(ase))) }
          case i =>
            monad.raiseError(i)
        }, a => {
          monad.tell(handler.buildMetaData) >>= { _ => monad.point(a) }
        })
    }
  }
}
