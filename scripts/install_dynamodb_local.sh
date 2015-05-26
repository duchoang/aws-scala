#!/bin/bash -e

options=':f'

while getopts $options option
do
    case $option in
        f   )   force=1;;
    esac
done

# find the timeout command ('timeout' on ubuntu, 'gtimeout' on MacOS X)
for cmd in gtimeout timeout; do
  if type ${cmd} > /dev/null 2>&1; then
    TIMEOUT_CMD=${cmd}
    break
  fi
done
if [ -z "${TIMEOUT_CMD}" ]; then
  echo "It seems you don't have the timeout binary. If you're on MacOS X, try 'brew install coreutils'.";
  exit 1
fi

JAR_HREF=http://dynamodb-local.s3-website-us-west-2.amazonaws.com/dynamodb_local_latest
SCRIPT_DIR=`dirname "$0"`
DYNAMO_DB_LIB_HOME="$SCRIPT_DIR/../../dynamodb"
TIMEOUT_SECONDS=20

if [[ ! -d "$DYNAMO_DB_LIB_HOME" ]] || [[ ! -z "$force" ]]; then
    # check if the dir was created by us to avoid race conditions
    mkdir -p "$DYNAMO_DB_LIB_HOME"
    if [ $? -eq 0 -o -n "$force" ]; then
        echo "Installing to ${DYNAMO_DB_LIB_HOME}"
        curl -sL "$JAR_HREF" | tar zx -C "$DYNAMO_DB_LIB_HOME"
        touch "$DYNAMO_DB_LIB_HOME"/.ready
    fi
fi

if [ \! -e "${DYNAMO_DB_LIB_HOME}"/.ready ]; then
    echo "Waiting for DynamoDB to become ready"
    ${TIMEOUT_CMD} --foreground ${TIMEOUT_SECONDS} bash -c "until test -e ${DYNAMO_DB_LIB_HOME}/.ready; do sleep 2; done; exit 0"
fi

