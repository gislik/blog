#!/bin/bash

echo "Building blog"
cabal run rebuild
echo "Syncing to GitHub Pages repository"
rsync -a --delete _site/* gislik.github.com
