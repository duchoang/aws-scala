# Scala wrapper for AWS SDK

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

    libraryDependencies += "io.atlassian.aws-scala" %% "aws-scala"  % "0.1.2"
    
If you want the `test` JAR for some useful helpers, at the moment you will need to import the individual modules and core, e.g.: 
    
    libraryDependencies ++= Seq(
        "io.atlassian.aws-scala" %% "aws-scala-core"  % "0.1.2",
        "io.atlassian.aws-scala" %% "aws-scala-s3"  % "0.1.2",
        "io.atlassian.aws-scala" %% "aws-scala-core"  % "0.1.2"  % "test" classifier "tests",
        "io.atlassian.aws-scala" %% "aws-scala-s3"  % "0.1.2"  % "test" classifier "tests",
        )
    
   
### Step 1 - Creating a client

The `io.atlassian.aws.AmazonClient` object has some useful functions for creating clients. So to create an S3 client you can do:

    import com.amazonaws.services.s3.AmazonS3Client                 // Yes this is ugly, someone please fix
    import io.atlassian.aws.AmazonClient
    
    val defaultClient = AmazonClient[AmazonS3Client].default        // Create a default client i.e. like  new AmazonS3Client
    
    val withEndpoint = AmazonClient[AmazonS3Client].withEndpoint("http://foo.com")  // Create a default client with the specified endpoint. This is probably most useful for creating a DynamoDB client talking to a local Dynamo DB
    
    val config = AmazonClientConnectionDef(...)
    val fallbackConfig = Some(AmazonClientConnectionDef(...))
    
    val withConfig = AmazonClient[AmazonS3Client].withClientConfiguration(config, fallbackConfig)   // Create an S3 client with config from `config`, filling in any unset config items using the fallback config. 

`AmazonClientConnectionDef` can be created programmatically or loaded via `kadai-config`. There is an `Accessor` already, so you just need to `import AmazonClientConnectionDef._` e.g.:

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

Each AWS resource has a typedef setting the client to the appropriate AWS SDK client type. i.e. `S3Action`, `DynamoDBAction`, `CFAction`.
Each resource also has an object that creates these actions e.g. `S3`, `DynamoDB` and `CloudFormation` objects. Where possible, we've created `scalaz` Tagged types to wrap primitive Strings.

#### DynamoDB table mapping

To use the DynamoDB actions, you will need to provide a mapping between your data you want to save and something that the AWS SDK understands.
Check out `io.atlassian.aws.dynamodb.TestData` in the `test` source tree for examples.

You'll need to:

   1. Create case classes for your `key` and `value` objects. There is no need for these to correspond directly to columns in your table. e.g. `ThingKey` and `ThingValue`   
   2. Provide a `TableDefinition` that defines the Dynamo table key types, table name and provisioned capacity (if you want to create a table). Use `TableDefinition.from` to help.
   3. Provide a `Marshaller` for the `key` class. e.g. `ThingKeyDynamoMarshaller`. There are a couple of ways to do it:
         1. Use `Marshaller.from` and specify the mapping manually via `Marshaller.set`. This is great for quick and dirty, but you probably want to use the option below. e.g. for the `ThingKey` example:
              `implicit val ThingKeyDynamoMarshaller =
                 Marshaller.from[ThingKey] { a =>
                   Map(
                     Marshaller.set("logicalKey", s"${a.tenant}/${a.app}/${a.blobId}"),
                     Marshaller.set("seq", a.seq)
                   )
                 }`
         2. Create `Column`s that map to Dynamo table columns, which needs an `Encoder` for the type. There are encoders for basic types already provided that you can build on. e.g. Check out `ThingHashKeyEncoder` and `ThingHashKeyDynamoMarshaller`
   4. If you're using queries and table with hash and range keys, you will need separate `Marshaller`s for the hash key and the range key. e.g. `ThingHashKeyDynamoMarshaller` and `ThingRangeKeyDynamoMarshaller`
   5. Provide a `Marshaller` and an `Unmarshaller` for your `value` class. `Unmarshaller`s are the opposite of marshallers, and you again have the option to construct them manually
      using `Unmarshaller.Operation.get` or using the `Column`s. e.g. `ThingValueDynamoMarshaller` and `ThingValueDynamoUnmarshaller`
   6. Provide a `StoreValue` for your `value` class. This represents what to do upon creating versus updating a value just in case you don't want
      to clobber other values. e.g. `ThingValueDynamoStoreValue` sets the `deletedTimestamp` value only upon a `delete` operation.
      There are useful combinators on `StoreValue` e.g. `StoreValue.newFromValues` just to put all the values.

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
          * Copy the scripts in the `scripts` directory for installing, starting and stopping local Dynamo.
          * Add a `arguments` constructor argument to your spec class i.e. `(val arguments: org.specs2.main.Arguments)`
          * Add an implicit value pointing to the client factory function i.e. `implicit val DYNAMO_CLIENT = dynamoClient`
          * Override the `scriptDirectory` function to point to where you store your scripts. e.g. `override val scriptDirectory = "../scripts"`. It is a relative path from the root of the module typically.
          * In your spec list, ensure you have steps to start local dynamo, create a test table, delete the test table and stop dynamo i.e.
            
                  ${Step(startLocalDynamoDB)}
                  ${Step(createTestTable)}
                  ${Step(deleteTestTable)}
                  ${Step(stopLocalDynamoDB)}
          
   * `io.atlassian.aws.SQSSpecOps` - Has a bunch of useful functions for creating and deleting temporary queues for testing. Check out `SQSSpec` to see how to use it.
   
If you want to spin up a local DynamoDB, you'll need to copy the `scripts` directory in this repository into your project. You'll need `gtimeout` on your box that is running the scripts (e.g. via `brew install coreutils` on Mac OS X).

### Step 3 - Profit

To run an action, just call `run` with a client, and then `run` on the Attempt to actually run it all. 

## TODO

  * Stop leaking AWS SDK client classes when creating an `AmazonClient`.
  * Better error handling. At the moment, we're just wrapping exceptions into `Invalid`s. We should probably provide standard failure case classes for various AWS error conditions e.g. not found, forbidden

## Developing

This project is a pretty standard Scala/SBT project using `specs2`. Have a look at the existing specs for examples. We are using immutable specs.

### local versus integration tests

There are a bunch of local tests for things that can be tested locally, and integration tests for things that need to talk to AWS resources.
DynamoDB specs are a little different in that the spec spins up a [local DynamoDB](http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tools.DynamoDBLocal.html)
in local mode, and in integration mode it talks with actual DynamoDB

To run the local tests, just do the normal `sbt test`.

To run integration tests, you will need to:

  1. Set up AWS access keys as per standard AWS Java SDK settings (e.g. `AWS_SECRET_KEY` and `AWS_ACCESS_KEY_ID` environment variables)
  2. Ensure that you have `gtimeout` installed e.g. `brew install coreutils` on Mac OS X
  3. Run the integration tests via `sbt 'test-only -- aws-integration'`
  

### Publishing and releasing

To release and publish, use the standard `sbt`-ism:

    sbt publish      # To publish a snapshot to maven private snapshot repo
    sbt release      # To tag a release and publish to maven private release repo
    
Obviously be sure the run the integration before releasing.
