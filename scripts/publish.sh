#!/bin/bash

echo "Syncing to GitHub Pages repository"
rsync -a --delete --exclude=.git _site/ gislik.github.io
echo "Adding new content and removing old"
git -C gislik.github.io add --all .
echo "Comitting changes"
git -C gislik.github.io commit -a -m "$$"
echo "Pushing repository to GitHub"
git -C gislik.github.io push origin master
