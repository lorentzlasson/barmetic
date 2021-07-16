#!/bin/bash
set -e

# build
elm make src/Main.elm

# deploy
git add index.html
git commit -m 'Deploy'
git push
