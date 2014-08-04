#!/bin/bash

echo "Comitting changes"
git -C gislik.github.com commit -a -m "$$"
echo "Pushing repository to GitHub"
git -C gislik.github.com push origin master
