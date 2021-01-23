#!/bin/bash

# include hidden files
shopt -s dotglob 

echo "Building blog"
stack run blog rebuild
echo "Syncing to GitHub Pages repository"
rsync -a --delete --exclude=.git _site/ gislik.github.io
