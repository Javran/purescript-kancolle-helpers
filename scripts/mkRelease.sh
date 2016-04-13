#!/bin/bash

stack exec -- ghc -O2 MkRelease.hs -main-is MkRelease && ./MkRelease
