#!/bin/bash

# Copyright (C) 2018 Johann Lee <me@qinka.pro>
#
# This file is part of Haskell-FAI
#
# Haskell-FAI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Haskell-FAI is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Haskell-FAI. If not, see <http://www.gnu.org/licenses/>.
#

echo
echo Build
echo

## LLVM
if [ -n "$LLVM" ]; then
    echo Enable LLVM
    export LLVM_FLAG=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi

## THREADED
if [ -n "$THREADED" ]; then
    echo Enable -threaded
    export THREADED_FLAG=" --ghc-options -threaded --ghc-options -with-rtsopts=-N "
fi 

## 
if [ -n "$DEBUG" ]; then
    echo Enable debug
    export XDEBUGFLAG=" --ghc-options -rtsopts=all -g"
else
    export XDEBUGFLAG=" --ghc-options -rtsopts=some --ghc-options --ghc-options --ghc-options -O3"
fi

cd $TRAVIS_BUILD_DIR
export FLAGS="$THREADED_FLAG $LLVM_FLAG $xDEBUGFLAG"
echo Using flags: $FLAGS
stack build $FLAGS