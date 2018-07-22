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
    export X_LLVM_FLAGS=" --ghc-options -fllvm --ghc-options -pgmlo --ghc-options opt-$LLVM --ghc-options -pgmlc --ghc-options llc-$LLVM "
fi

## THREADED
if [ -n "$THREADED" ]; then
    echo Enable -threaded
    export X_THREADED_FLAGS=" --ghc-options -threaded --ghc-options -with-rtsopts=-N "
fi 

## 
if [ -n "$DEBUG" ]; then
    echo Enable debug
    export X_DEBUG_FLAGS=" --ghc-options -rtsopts=all --ghc-options -g"
else
    export X_DEBUG_FLAGS=" --ghc-options -rtsopts=some --ghc-options --ghc-options --ghc-options -O3"
fi

if [ -n "$CUDA" ]; then
    echo Enable CUDA
    export X_CUDA_FLAGS=" "
else
    echo Disable CUDA
    export X_CUDA_FLAGS=" --flag FAI:-enable-cuda "
fi

cd $TRAVIS_BUILD_DIR
export FLAGS="$X_THREADED_FLAGS $X_LLVM_FLAGS $X_DEBUG_FLAGS $X_CUDA_FLAGS"
echo Using flags: $FLAGS
stack build $FLAGS

echo
echo build examples
cd $TRAVIS_BUILD_DIR/example/example-blas
mkdir build
cd build
export EBLAS_BUILD_DIR=`pwd`
cmake  -DBUILD_TESTS=On -DENABLE_OPENMP=On ..
make
cd $TRAVIS_BUILD_DIR