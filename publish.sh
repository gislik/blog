#!/bin/bash

echo "Adding new content and removing old"
git -C gislik.github.io add --all .
echo "Comitting changes"
git -C gislik.github.io commit -a -m "$$"
echo "Pushing repository to GitHub"
git -C gislik.github.io push origin master
