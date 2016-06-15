# Scala wrapper for AWS SDK

[ ![Codeship Status for atlassian/aws-scala](https://codeship.com/projects/05f9e740-11cf-0133-9628-6695b09e6893/status?branch=master)](https://codeship.com/projects/92367)

Dealing with the Java AWS SDK is messy. This library attempts to make it less messy by:

 * Making it easier to create AWS SDK clients based on configuration and supporting fallback configurations
 * Providing a nicer syntax for AWS SDK calls by wrapping them in a monad (no thrown exceptions, and sequencing!)
 * Tries to provide Options where null can be returned
 * Provides a nice 'ORM' for DynamoDB table access. Dealing with `AttributeValue`s is ugly, real ugly.
 * Also provide a nice 'ORM' for SQS message marshalling/unmarshalling.

Currently the library has basic support for S3, DynamoDB, CloudFormation and SQS. Feel free to add more as you need it.

## Usage

### Step 0 - Adding the dependency

The project is split into separate modules for each type of AWS API so you can import them separately if you wish (e.g. `aws-scala-s3`, `aws-scala-dynamodb`, `aws-scala-sqs`, `aws-scala-cloudformation`).
Alternatively, you can:

    libraryDependencies += "io.atlassian.aws-scala" %% "aws-scala"  % "5.0.0"
    
If you want the `test` JAR for some useful helpers, at the moment you will need to import the individual modules and core, e.g.: 
    
    libraryDependencies ++= Seq(
        "io.atlassian.aws-scala" %% "aws-scala-core"  % "5.0.0",
        "io.atlassian.aws-scala" %% "aws-scala-s3"  % "5.0.0",
        "io.atlassian.aws-scala" %% "aws-scala-core"  % "5.0.0"  % "test" classifier "tests",
        "io.atlassian.aws-scala" %% "aws-scala-s3"  % "5.0.0"  % "test" classifier "tests",
        )

Version 4.x works with Scalaz 7.1. Version 5.x will work with Scalaz 7.2. Both series are cross-compiled for Scala 2.10 and 2.11.

Be sure the check the CHANGELOG.md for a list of breaking changes.
   
### Step 1 - Creating a client

The `io.atlassian.aws.AmazonClient` object has some useful functions for creating clients. So to create an S3 client you can do:

    import com.amazonaws.services.s3.AmazonS3Client                 // Yes this is ugly, someone please fix
    import io.atlassian.aws.AmazonClient
    
    val defaultClient = AmazonClient[AmazonS3Client].default        // Create a default client i.e. like  new AmazonS3Client
    
    val withEndpoint = AmazonClient[AmazonS3Client].withEndpoint("http://foo.com")  // Create a default client with the specified endpoint. This is probably most useful for creating a DynamoDB client talking to a local Dynamo DB
    
    val config = AmazonClientConnectionDef(...)
    val fallbackConfig = Some(AmazonClientConnectionDef(...))
    
    val withConfig = AmazonClient[AmazonS3Client].withClientConfiguration(config, fallbackConfig)   // Create an S3 client with config from `config`, filling in any unset config items using the fallback config. 

`AmazonClientConnectionDef` can be created programmatically or loaded via [kadai-config](https://bitbucket.org/atlassian/kadai). There is an `Accessor` already, so you just need to `import AmazonClientConnectionDef._` e.g.:

Config settings:

      aws-client {             # Connection settings for all AWS Clients used in the server. Can be overridden with specific settings for relevant components
        connection-timeout-ms = # Connection timeout in ms
        region =               # aws region name.
        proxy-host =           # Proxy host to go through to access AWS resources
        proxy-port =           # Port of proxy host to go through to access AWS resources
        socket-timeout-ms =    # com.amazonaws.ClientConfiguration#setSocketTimeout
        max-error-retry =      # com.amazonaws.ClientConfiguration#setMaxErrorRetry
        max-connections =      # com.amazonaws.ClientConfiguration#setMaxConnections
      }

    import AmazonClientConnectionDef._
    import kadai.config.Configuration
    
    val foo = Configuration.load("path/to/config/file").option[AmazonClientConnectionDef]("aws-client")
    
    
There is also some useful bits in `io.atlassian.aws.AmazonRegion` and an `Accessor` too in `io.atlassian.aws.AmazonRegionDef` if you need a region e.g. you want to set the region on a client.

### Step 2 - Creating actions

The basic pattern for accessing AWS resources is to create `Action` instances via.

There is a base `AwsAction` monad that is basically a reader monad that takes in a client and produces a `kadai.Attempt` (think better Scala `Try`) that when run will perform the operation safely i.e. returns either an `Invalid` for errors or the result from the operation.
Being a monad, you can sequence them, `run` them with the client, and also `recover` or `handle` invalid cases much like Scala `Futures`. 

Each AWS resource has a typedef setting the client to the appropriate AWS SDK client type. i.e. `S3Action`, `CFAction`. NB DynamoDB is a little different, see below.
Each resource also has an object that creates these actions e.g. `S3` and `CloudFormation` objects. Where possible, we've created `scalaz` Tagged types to wrap primitive Strings.

#### Using tagged types

We use tagged types in quite a few places to make sure strings like bucket names and key names can't be accidentally mixed up. With changes to Scalaz 7.1 tagged types, we've added some
auto-converters to unwrap tagged types. To access these, you will need to import the contents of companion objects of types e.g. `import Bucket._, S3Key._`

#### Using DynamoDB

Like the other AWS resources, there is a `DynamoDBAction` that you run with the appropriate AWS client, however creating these actions is a little different.
You use the `Table` algebra instead. Check out `io.atlassian.aws.dynamodb.TestData` and `io.atlassian.aws.dynamodb.TableSpec` in the `test` source tree for examples.

As a summary:

   1. Create case classes for your `key` and `value` objects. There is no need for these to correspond directly to columns in your table. e.g. `ThingKey` and `ThingValue`
   2. Extend `io.atlassian.aws.dynamodb.Table` specifying the key `K` and value `V` types.
   3. Provide a `TableDefinition` that defines the Dynamo table key types, table name and provisioned capacity (if you want to create a table). Use `TableDefinition.from` to help.
   4. Provide a `Column` for each column in your DynamoDB table. This basically specifies a name for the column and a Scala type to we can map to that column.
   5. Provide composite `Column`s via the `Column.composeX` functions to map you high-level Scala types (i.e. your `key` and `value` types) to the columns defined in the preceding step.
   6. Access your table by:
         1. Creating `DBOp`s through your `Table`'s `get`/`put`/... functions
         2. Run the `DBOp`s by using the `DynamoDB.interpreter` to get `DynamoDBAction`, which you can then run with your AWS client. 

#### SQS message marshallers and unmarshallers

To use the `send` and `receive` functions in SQS, you need to provide a `Marshaller` and `Unmarshaller` for your message class. Check out `io.atlassian.aws.sqs.Examples` for an example. You will basically need to:

   * Provide a way of encoding/decoding your message body. JSON is a good way using Argonaut.
   * Providing a `Marshaller` and an `Unmarshaller`, using the combinators in the `Marshaller` and `Unmarshaller`. Typically there needs to be a way of encoding/decoding both the message body and optionally message 'headers'.
   
There is a wrapper `RetriedMessage` case class that adds a `retryCount` to messages, which is stored as a message attribute i.e. header. That provides a nice example of how marshallers/unmarshallers can be combined.

#### Using test helpers

The `test` JAR includes a few useful helpers:

   * `io.atlassian.aws.S3SpecOps` - Has a bunch of useful functions for creating and deleting temporary buckets for testing. Check out `S3Spec` to see how to use it.
   * `io.atlassian.aws.DynamoDBSpecOps` and `io.atlassian.aws.LocalDynamoDBSpec` - Has a bunch of useful functions for creating and deleting temporary tables for testing, and also spinning up a local DynamoDB. Check out `DynamoDBSpec` to see how to use it. 
      If you want to use `LocalDynamoDBSpec`, the key things to do are:
          * Extend the trait
          * Copy the scripts in the `scripts` directory for installing, starting and stopping local Dynamo. You will need to have `timeout/gtimeout` and `wget` installed as well as `npm` if you want to use [dynalite](https://github.com/mhart/dynalite) instead of AWS's local DynamoDB (needed if you use JSON encoding).
          * Add a `arguments` constructor argument to your spec class i.e. `(val arguments: org.specs2.main.Arguments)`
          * Add an implicit value pointing to the client factory function i.e. `implicit val DYNAMO_CLIENT = dynamoClient`
          * Override the `scriptDirectory` function to point to where you store your scripts. e.g. `override val scriptDirectory = "../scripts"`. It is a relative path from the root of the module typically.
          * **If you want to use dynalite instead of AWS's Local DynamoDB, then override `useAwsLocalDynamo` to be `false`. You will need to do this if you use JSON encoding for any columns.** 
          * In your spec list, ensure you have steps to start local dynamo, create a test table, delete the test table and stop dynamo i.e.
            
                  ${Step(startLocalDynamoDB)}
                  ${Step(createTestTable)}
                  ${Step(deleteTestTable)}
                  ${Step(stopLocalDynamoDB)}
          
   * `io.atlassian.aws.SQSSpecOps` - Has a bunch of useful functions for creating and deleting temporary queues for testing. Check out `SQSSpec` to see how to use it.
   
If you want to spin up a local DynamoDB, you'll need to copy the `scripts` directory in this repository into your project. You'll need `gtimeout`, `wget` and `npm` on your box that is running the scripts (e.g. via `brew install coreutils` on Mac OS X).

### Step 3 - Profit

To run an action, just call `run` with a client, and then `run` on the Attempt to actually run it all. 

## TODO

  * Stop leaking AWS SDK client classes when creating an `AmazonClient`.
  * Better error handling. At the moment, we're just wrapping exceptions into `Invalid`s. We should probably provide standard failure case classes for various AWS error conditions e.g. not found, forbidden

## Developing

This project is a pretty standard Scala/SBT project using `specs2`. Have a look at the existing specs for examples. We are using immutable specs.

### Scalariform

For consistent code formatting, we're using Scalariform. There is a Git pre-commit hook under `project/hooks` that you can/should put into
your `.git/hooks` directory that will run Scalariform before all commits.

### local versus integration tests

There are a bunch of local tests for things that can be tested locally, and integration tests for things that need to talk to AWS resources.
DynamoDB specs are a little different in that the spec spins up a [local DynamoDB](http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tools.DynamoDBLocal.html)
in local mode (or [dynalite](https://github.com/mhart/dynalite) for JSON encoding tests), and in integration mode it talks with actual DynamoDB

To run the local tests, just do the normal `sbt test`.

To run integration tests, you will need to:

  1. Set up AWS access keys as per standard AWS Java SDK settings (e.g. `AWS_SECRET_KEY` and `AWS_ACCESS_KEY_ID` environment variables)
  2. Ensure that you have `gtimeout`, `wget` and `npm` installed e.g. `brew install coreutils` and `brew install wget` on Mac OS X
  3. Run the integration tests via `sbt 'test-only -- aws-integration'`
  

### Publishing and releasing

To release and publish, use the standard `sbt`-ism:

    sbt publish             # To publish a snapshot to maven private snapshot repo
    sbt 'release cross'     # To tag a release and publish to maven private release repo
    
Obviously be sure the run the integration before releasing.

Internally in Atlassian, we have a build and release pipeline on [Bamboo](https://engservices-bamboo.internal.atlassian.com/browse/OSSC-AWSSCALA), hopefully to be made public at some point soon.

