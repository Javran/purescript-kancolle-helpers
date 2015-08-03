#!/bin/bash

export CI=true

# assume we are in PROJECT_HOME
export PROJECT_HOME=`pwd`
export PURESCRIPT_HOME="$HOME/purescript"

mkdir -p "$PURESCRIPT_HOME"

pushd "$PURESCRIPT_HOME"

# download and compiler purescript's compiler
curl 'https://codeload.github.com/purescript/purescript/zip/master' -o 'purescript-master.zip' -L
unzip purescript-master.zip

pushd purescript-master
cabal update
cabal install happy
cabal install alex
cabal install --only-dependencies
cabal install
popd # purescript-master

popd # $PURESCRIPT_HOME

# now we are ready to build the project
cd "$PROJECT_HOME"

# dependencies
npm install pulp
npm install uglify-js

pulp dep i
