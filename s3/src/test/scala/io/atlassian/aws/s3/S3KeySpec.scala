package io.atlassian.aws.s3

import org.scalacheck.Prop
import org.specs2.{ScalaCheck, SpecificationWithJUnit}
import scalaz.syntax.std.list._

class S3KeySpec extends SpecificationWithJUnit with S3Arbitraries with ScalaCheck {
  import S3Key._

  def is = s2"""
    S3Key
      .isFolder should return true for folders              $s3KeyIsFolderReturnsTrueForFolders
      .isFolder should return false for files               $s3KeyIsFolderReturnsFalseForFiles

      .prefix for folder should return itself               $s3KeyPrefixForFolderReturnsSelf
      .prefix for key with no folders should return empty string $s3KeyPrefixWorksForNoFolders
      .prefix for key with folders should work              $s3KeyPrefixWorksForFolders

      .folders for folder should include all path components $foldersForFolderReturnsAllComponents
      .folders for key with no folders should return no folders $foldersForKeyWithNoFoldersWorks
      .folders for key with folders should return folders    $foldersForKeyWithFoldersWorks

      .foldersWithLeadingPaths should
        work for key with no folders                        $foldersWithLeadingPathsNoFolders
        work for key with folders                           $foldersWithLeadingPathsFolders
  """

  def s3KeyIsFolderReturnsTrueForFolders = Prop.forAll { key: String =>
    S3Key(s"$key/").isFolder must beTrue
  }

  def s3KeyIsFolderReturnsFalseForFiles = Prop.forAll {
    (key: String) =>
      S3Key(s"${key.stripSuffix("/")}").isFolder must beFalse
  }

  def s3KeyPrefixForFolderReturnsSelf = Prop.forAll {
    (key: S3Key, folders: S3Folders) =>
      S3Key(folders.toPrefix).prefix === s"${folders.toPrefix}"
  }

  def s3KeyPrefixWorksForNoFolders = Prop.forAll {
    key: S3Key =>
      S3Key(key.split("/").last).prefix === ""
  }

  def s3KeyPrefixWorksForFolders = Prop.forAll {
    (key: S3Key, folders: S3Folders) =>
      S3Key(folders.folders, key).prefix === s"${
        folders.folders.mkString("/")
      }/"
  }

  def foldersForFolderReturnsAllComponents = Prop.forAll {
    (folders: S3Folders) =>
      S3Key(folders.toPrefix).folders === folders.folders
  }

  def foldersForKeyWithNoFoldersWorks = Prop.forAll {
    key: S3Key =>
      key.folders === Nil
  }

  def foldersForKeyWithFoldersWorks = Prop.forAll {
    (folders: S3Folders, key: S3Key) =>
      S3Key(folders.folders, key).folders === folders.folders
  }

  def foldersWithLeadingPathsNoFolders = Prop.forAll {
    (key: S3Key) =>
      key.foldersWithLeadingPaths === Nil
  }

  def foldersWithLeadingPathsFolders = Prop.forAll {
    (key: S3Key, folders: S3Folders) =>
      S3Key(folders.folders, key).foldersWithLeadingPaths.sorted ===
        folders.folders.initz.tail.map { _.mkString("/") }.sorted
  }

}
