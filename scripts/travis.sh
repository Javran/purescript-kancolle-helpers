#!/bin/bash

set -x

cabal update
cabal install happy --force-reinstalls
cabal install alex --force-reinstalls
cabal install purescript-0.7.2.0 --force-reinstalls

# dependencies
npm install pulp
npm install uglify-js

pulp dep i
