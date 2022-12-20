#!/bin/bash

if [ -z $1 ]; then
  echo "Usage: $0 <tag>"
  exit 1
fi

tag=$1

exec docker build . -t gislik/blog:$tag