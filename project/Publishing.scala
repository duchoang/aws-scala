// Copyright 2012 Atlassian PTY LTD
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import sbt._
import Keys._

object Publishing extends Plugin {
  val nexus = "https://maven.atlassian.com/"
  lazy val release = Some("releases" at nexus + "private")
  lazy val snapshots = Some("snapshots" at nexus + "private-snapshot")
  lazy val local = Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

  override def settings = 
    Seq(
      publishTo <<= version { (v: String) =>
        if (v.trim endsWith "SNAPSHOT")
          local
        else
          release
      }
      , publishMavenStyle := true
      , publishArtifact in Test := true
      , pomExtra :=
        <scm>
          <url>https://stash.atlassian.com/projects/ES/repos/aws-scala</url>
          <connection>scm:ssh://git@stash.atlassian.com:7997/ES/aws-scala.git</connection>
          <developerConnection>scm:git:ssh://git@stash.atlassian.com:7997/ES/aws-scala.git</developerConnection>
        </scm>
          <issueManagement>
            <system>JIRA</system>
            <url>https://sdog.jira.com/secure/RapidBoard.jspa?rapidView=93</url>
          </issueManagement>

      , pomIncludeRepository := { (repo: MavenRepository) => false } // no repositories in the pom
    )
}
