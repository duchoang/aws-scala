package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model._
import kadai.Invalid
import scala.concurrent.duration._
import scalaz.{ NaturalTransformation, ~>, Functor, \/ }
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.id._
import scalaz.syntax.std.option._
import scala.collection.JavaConverters._

object SQS {
  import SQSAction._

  def createQueue(parameters: QueueParameters): SQSAction[QueueURL] =
    SQSAction.withClient {
      _.createQueue {
        new CreateQueueRequest(parameters.name) <| {
          _.withAttributes(parameters.attributes).withQueueName(parameters.name)
        }
      } |> { res => QueueURL(res.getQueueUrl) }
    }

  def queueURL(name: String): SQSAction[QueueURL] =
    SQSAction.withClient { _.getQueueUrl(name) }.map { r => QueueURL(r.getQueueUrl) }

  def safeQueueURL(name: String): SQSAction[Option[QueueURL]] =
    queueURL(name).map { _.some }.handle {
      case Invalid.Err(e: QueueDoesNotExistException) => SQSAction.ok(None)
    }

  def deleteQueue(url: QueueURL): SQSAction[Unit] =
    SQSAction.withClient { _.deleteQueue(url.unwrap) |> { _ => () } }

  def send[A: Marshaller](url: QueueURL, message: A, delay: Duration = 0.seconds): SQSAction[SendResult] =
    SQSAction.withClient {
      _.sendMessage(
        new SendMessageRequest()
          .withQueueUrl(url.unwrap)
          .withDelaySeconds(delay.toSeconds.toInt)
          .withMessageAttributes(Marshaller[A].headerFlattened(message).asJava)
          .withMessageBody(Marshaller[A].body(message))
      ) |> { res => SendResult(MessageId(res.getMessageId)) }
    }

  def receive[A: Unmarshaller](url: QueueURL, params: ReceiveMessageParameters = ReceiveMessageParameters()): SQSAction[List[ReceivedMessage[A]]] =
    SQSAction.withClient {
      _.receiveMessage {
        new ReceiveMessageRequest(url.unwrap)
          .withMaxNumberOfMessages(params.numMessages)
          .withAttributeNames("All")
          .withMessageAttributeNames("All") <| { r =>
            params.visibilityTimeout.foreach { t => r.setVisibilityTimeout(t.toSeconds.toInt) }
            params.waitTime.foreach { t => r.setWaitTimeSeconds(t.toSeconds.toInt) }
          }
      }.getMessages.asScala.toList.map {
        m => Unmarshaller.receivedMessage[A].unmarshall(m).toOr.valueOr(ReceivedMessage.Invalid(m, _))
      }
    }

  def delete(url: QueueURL, handle: ReceiptHandle): SQSAction[Unit] =
    SQSAction.withClient {
      _.deleteMessage(new DeleteMessageRequest().withQueueUrl(url.unwrap).withReceiptHandle(handle.unwrap)) |> { _ => () }
    }

  def delete(url: QueueURL, handles: List[ReceiptHandle]): SQSAction[DeleteResult] = {

    def deleteBatch(batch: List[ReceiptHandle]): SQSAction[List[FailedDelete]] =
      SQSAction.withClient { client =>

        val batchWithId = batch.zipWithIndex.map { case (h, index) => index.toString -> h }
        val batchRequestEntries = batchWithId.map { case (index, h) => new DeleteMessageBatchRequestEntry(index, h.unwrap) }
        val idToHandle = batchWithId.toMap

        client.deleteMessageBatch(new DeleteMessageBatchRequest(url.unwrap)
          .withEntries(batchRequestEntries.asJava)).getFailed.asScala.map { entry =>
          FailedDelete(idToHandle.get(entry.getId), entry.getSenderFault, entry.getMessage)
        }.toList
      }

    handles.grouped(10).map { deleteBatch }.toList.sequence.map { _.flatten }.map { l => DeleteResult(handles, l) }
  }

  def changeVisibility(url: QueueURL, handle: ReceiptHandle, newVisibilityFromNow: Duration): SQSAction[Unit] = {
    SQSAction.withClient {
      _.changeMessageVisibility(
        new ChangeMessageVisibilityRequest()
          .withQueueUrl(url.unwrap)
          .withReceiptHandle(handle.unwrap)
          .withVisibilityTimeout(newVisibilityFromNow.toSeconds.toInt)
      ) |> { _ => () }
    }
  }
}
