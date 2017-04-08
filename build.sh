#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -it \
  -v $(pwd)/../reesd-client:/home/gusdev/reesd-client \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install reesd-client/reesd-client.cabal
