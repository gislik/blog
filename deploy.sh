#!/bin/bash

echo "Adding new content and removing old"
git -C gislik.github.com add --all .
echo "Comitting changes"
git -C gislik.github.com commit -a -m "$$"
echo "Pushing repository to GitHub"
git -C gislik.github.com push origin master
