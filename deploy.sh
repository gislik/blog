#!/bin/bash

#cp -Rp _site/* gislik.github.com
rsync -a --delete _site/* gislik.github.com
