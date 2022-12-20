#!/bin/bash

cmd=$1 || build

exec docker run -t --rm -v $PWD:/content -w /content gislik/blog:latest $cmd
