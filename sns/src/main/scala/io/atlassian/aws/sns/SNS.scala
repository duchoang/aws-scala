package io.atlassian.aws
package sns

import com.amazonaws.services.sns.model.PublishRequest
import scalaz.syntax.id._

object SNS {

  def createTopic(name: String): SNSAction[TopicARN] =
    SNSAction.withClient {
      _.createTopic(name) |> { res => TopicARN(res.getTopicArn) }
    }

  def deleteTopic(arn: TopicARN): SNSAction[Unit] =
    SNSAction.withClient {
      _.deleteTopic(arn.unwrap) |> { _ => () }
    }

  def sendMessage[A: Marshaller](topic: TopicARN, message: A): SNSAction[MessageId] =
    SNSAction.withClient {
      _.publish {
        val req = new PublishRequest().withTopicArn(topic.unwrap).withMessage(Marshaller[A].message(message))
        Marshaller[A].subject(message).foreach { req.setSubject }
        req
      } |> { res => MessageId(res.getMessageId) }
    }
}
