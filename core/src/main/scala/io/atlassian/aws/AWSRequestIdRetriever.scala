package io.atlassian.aws

import com.amazonaws.handlers.RequestHandler2
import com.amazonaws.{ AmazonServiceException, Request, Response }

import scala.collection.concurrent.{ Map, TrieMap }
import scalaz.syntax.std.option._

object AWSRequestIdRetriever {
  import AwsAction._

  type RequestId = String

  case class Value(metaDataKey: String, requestId: String)

  private val contexts: Map[Int, Value] = TrieMap[Int, Value]()

  val requestHandler = new RequestHandler2 {
    override def afterError(request: Request[_], response: Response[_], e: Exception) =
      (for {
        header <- headerKey(request.getServiceName)
        key <- metaDataKey(request.getServiceName)
        id <- e match {
          case ase: AmazonServiceException => ase.getRequestId.some
          case _ => retriever(header, response)
        }
      } yield Value(key, id)) foreach { v =>
        contexts.put(e.hashCode, v)
      }

    override def afterResponse(request: Request[_], response: Response[_]) =
      (for {
        header <- headerKey(request.getServiceName)
        key <- metaDataKey(request.getServiceName)
        id <- retriever(header, response)
      } yield Value(key, id)) foreach { v =>
        contexts.put(response.getAwsResponse.hashCode, v)
      }

    override def beforeRequest(request: Request[_]) = ()
  }


  // A must be whatever the aws client returns
  def withClient[C, A](f: C => A): AwsAction[C, A] =
    AwsAction.withMetaData { client: C =>
      try {
        val a = f(client)
        contexts.remove(a.hashCode).cata( v => (metaData(v), Attempt.ok(a)), (MetaData.none, Attempt.ok(a)) )
      } catch {
        case util.control.NonFatal(t) =>
          contexts.remove(t.hashCode).cata( v => (metaData(v), Attempt.exception(t)), (MetaData.none, Attempt.exception(t)) )
      }
    } recover {
      AmazonExceptions.transformException andThen invalid[C, A]
    }

  private val headerKey: String => Option[String] = {
    case S3.serviceName => S3.headerKey
    case DynamoDb.serviceName => DynamoDb.headerKey
    case _ => None
  }

  private val metaDataKey: String => Option[String] = {
    case S3.serviceName => S3.metaDataKey
    case DynamoDb.serviceName => DynamoDb.metaDataKey
    case _ => None
  }

  private def retriever(headerKey: String, res: Response[_]) =
    Option(res.getHttpResponse.getHeaders.get(headerKey))

  private def metaData(v: Value): MetaData =
    MetaData(v.metaDataKey, v.requestId)

  object S3 {
    val serviceName = "AmazonS3"
    val headerKey   = "x-amz-request-id".some
    val metaDataKey = "S3-AWS-REQUEST-ID".some
  }
  object DynamoDb {
    val serviceName = "AmazonDynamoDBv2"
    val headerKey   = "x-amzn-RequestId".some
    val metaDataKey = "DYNAMO-AWS-REQUEST-ID".some
  }

}
