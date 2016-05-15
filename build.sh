#! /usr/bin/env bash

docker run \
  -v $(pwd)/../reesd-client:/home/gusdev/reesd-client \
  images.reesd.com/reesd/stack:7.8.4 \
  cabal install reesd-client/reesd-client.cabal
