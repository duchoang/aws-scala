/**
 * Atlassian Bamboo Plan template for aws-scala. Use this to create a build plan in Bamboo to build, test and release aws-scala.
 *
 * Linked repository to atlassian/aws-scala repository needs to be configured with 'exclude changesets' set to the following
 * regex:
 *
 *   \[sbt-release\].*
 *   
 */

plan(key:'AWSSCALA',name:'AWS Scala') {
   project(key:'OSSC',name:'Open Source Scala')
   
   repository(name:'AWS Scala')
   
   trigger(type:'polling',description:'60 second chain trigger',
      strategy:'periodically',frequency:'60') {      
      repository(name:'AWS Scala')
      
   }
   stage(name:'Build and test') {
      job(key:'SBT',name:'SBT') {         
         requirement(key:'sbt',condition:'exists')
         
         task(type:'checkout',description:'Checkout Default Repository')
         
         task(type:'script',description:'SBT',scriptBody:'''
#!/bin/bash
 
#https://extranet.atlassian.com/jira/browse/BUILDENG-2995
export JAVA_HOME=${bamboo.capability.system.jdk.JDK 1.8}
#https://extranet.atlassian.com/jira/browse/BUILDENG-7018
export SBT_OPTS="-Dsbt.log.noformat=true -J-XX:MaxPermSize=512M -sbt-dir /opt/bamboo-agent/.sbt -d"
./sbt clean +test -J-Xmx2G
''')

         task(type:'jUnitParser',description:'Parse test results',
              final:'true',resultsDirectory:'**/test-reports/*.xml')

      }
   }

   stage(name:'Release',description:'Release and publish artifacts',
           manual:'true') {
      job(key:'REL',name:'Release') {
         requirement(key:'sbt',condition:'exists')

         task(type:'checkout',description:'Checkout Default Repository') {
            repository(name:'AWS Scala')
         }

         task(type:'script',description:'Set up remote tracking for push',
                 scriptBody:'''
#!/bin/bash

git remote set-url origin git@bitbucket.org:atlassian/aws-scala.git
git fetch origin -v
git branch --set-upstream $bamboo_planRepository_branch origin/$bamboo_planRepository_branch

''')

         task(type:'script',description:'SBT',scriptBody:'''
#!/bin/bash

USER=$(fgrep "user=" ~/.ivy2/.credentials | cut -d= -f2)
PWD=$(fgrep "password=" ~/.ivy2/.credentials | cut -d= -f2)


#https://extranet.atlassian.com/jira/browse/BUILDENG-2995
export JAVA_HOME=${bamboo.capability.system.jdk.JDK 1.8}
#https://extranet.atlassian.com/jira/browse/BUILDENG-7018
export SBT_OPTS="-Dsbt.log.noformat=true -J-XX:MaxPermSize=512M -sbt-dir /opt/bamboo-agent/.sbt -d"

./sbt -Dsbt.log.noformat=true ";set credentials:=Seq(Credentials(\\"Sonatype Nexus Repository Manager\\", \\"nexus-atlassian-central.buildeng.atlassian.com\\", \\"${USER}\\", \\"${PWD}\\")); set useGpg := true; set pgpPassphrase := Some(Array()); release with-defaults" -J-Xmx2G
''')

         task(type:'jUnitParser',description:'Parse test results',
                 final:'true',resultsDirectory:'**/test-reports/*.xml')

      }
   }

   branchMonitoring() {      
      createBranch(matchingPattern:'.*')
      inactiveBranchCleanup(periodInDays:'30')
      deletedBranchCleanup(periodInDays:'30')
   }
   
   dependencies(triggerOnlyAfterAllStagesGreen:'true',triggerForBranches:'true')
}

