#!/bin/bash

# include hidden files
shopt -s dotglob

export DRAFTS=${DRAFTS:-"false"}

echo "Building blog"
stack run blog rebuild
