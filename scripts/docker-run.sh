#!/bin/bash

cmd=$1 || build
tag=${2:-"latest"}

exec docker run -t --rm -v $PWD:/content -w /content gislik/blog:$tag $cmd
