#!/bin/bash

cmd=$1 || build
tag=${2:-"latest"}

DRAFTS=${DRAFTS:-"false"}

exec docker run -t --rm -v $PWD:/content -e DRAFTS=$DRAFTS -w /content gislik/blog:$tag $cmd
