Change log
===

## From version 6.x to 7.0.0
### Breaking changes:
* Upgrade from Scalaz 7.1 to 7.2.3.
* Upgrade from Specs2 3.6.6 to 3.8.3.
* `AwsAction.runAction` and `AwsAction.runActionWithMetaData` have been renamed to `unsafePerformAction` and `unsafePerformActionWithMetaData` to be more inline with scalaz.
