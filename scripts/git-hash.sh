#!/bin/sh
echo "export const GIT_HASH = '$(git rev-parse origin/master)';" > ./src/gitHash.js
