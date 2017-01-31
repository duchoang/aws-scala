package io.atlassian.aws
package cloudformation

import java.io.File
import com.amazonaws.services.cloudformation.model.{ ListStacksRequest, UpdateStackRequest, Stack, DescribeStacksRequest, DeleteStackRequest, CreateStackRequest }
import java.net.URL
import scala.io.Source
import scalaz.syntax.id._

object CloudFormation {

  def createOrUpdateStackFrom(file: File)(name: StackName): CFAction[StackOperationId] =
    stackExists(name).flatMap {
      case false => createStackFrom(file)(name)
      case true  => updateStackFrom(file)(name)
    }

  def createOrUpdateStackFrom(url: URL)(name: StackName): CFAction[StackOperationId] =
    stackExists(name).flatMap {
      case false => createStackFrom(url)(name)
      case true  => updateStackFrom(url)(name)
    }

  def createStackFrom(file: File)(name: StackName): CFAction[StackOperationId] =
    CFAction { client =>
      Attempt.safe {
        val fileSource = Source.fromFile(file)
        try {
          client.createStack(
            new CreateStackRequest().withStackName(name.unwrap).withTemplateBody(fileSource.mkString)
          ).getStackId |> StackOperationId.apply
        } finally {
          fileSource.close()
        }
      }
    }

  def createStackFrom(url: URL)(name: StackName): CFAction[StackOperationId] =
    CFAction.withClient {
      _.createStack(new CreateStackRequest().withStackName(name.unwrap).withTemplateURL(url.toString)).getStackId |> StackOperationId.apply
    }

  def updateStackFrom(file: File)(name: StackName): CFAction[StackOperationId] =
    CFAction { client =>
      Attempt.safe {
        val fileSource = Source.fromFile(file)
        try {
          client.updateStack(
            new UpdateStackRequest().withStackName(name.unwrap).withTemplateBody(fileSource.mkString)
          ).getStackId |> StackOperationId.apply
        } finally {
          fileSource.close()
        }
      }
    }

  def updateStackFrom(url: URL)(name: StackName): CFAction[StackOperationId] =
    CFAction.withClient {
      _.updateStack(new UpdateStackRequest().withStackName(name.unwrap).withTemplateURL(url.toString)).getStackId |> StackOperationId.apply
    }

  def deleteStack(name: StackName): CFAction[Unit] =
    CFAction.withClient { c =>
      c.deleteStack(new DeleteStackRequest().withStackName(name.unwrap)); ()
    }

  def describeStack(name: StackName): CFAction[Option[Stack]] =
    CFAction.withClient {
      import collection.JavaConverters._
      _.describeStacks(new DescribeStacksRequest().withStackName(name.unwrap)).getStacks.asScala.headOption
    }

  def stackExists(name: StackName): CFAction[Boolean] =
    CFAction.withClient {
      import collection.JavaConverters._
      _.listStacks(new ListStacksRequest()).getStackSummaries.asScala.exists { _.getStackName == name }
    }
}
