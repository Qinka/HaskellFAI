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

##########################################################
##
## Build Haskell FAI
##
##########################################################
function build_FAI() {
    echo FAI task
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
}

##########################################################
##
## Build example of blas
##
##########################################################
function build_example_blas() {
    echo build example of blas
    cd $TRAVIS_BUILD_DIR/example/example-blas
    mkdir build
    cd build
    export EBLAS_BUILD_DIR=`pwd`

    local TEST_STATUS ACC_CMAKE_FLAGS
    # test enable
    if [ -n "$RUN_TEST" ]; then
        TEST_STATUS=On
    else
        TEST_STATUS=Off
    fi

    case $ACC_BACKEND in
        OpenMP)
            ACC_CMAKE_FLAGS="-DENABLE_OPENMP=On"
            ;;
        OpenMP-target)
            ACC_CMAKE_FLAGS="-DENABLE_OPENMP=On -DENABLE_OPENMP_TARGET=On"
            ;;
        OpenACC)
            ACC_CMAKE_FLAGS="-DENABLE_OPENMP=Off -DENBALE_OPENACC=On"
            ;;
        *)
            ACC_CMAKE_FLAGS="-DENABLE_OPENMP=Off"
            ;;
    esac


    cmake -DBUILD_TESTS=$TEST_STATUS $ACC_CMAKE_FLAGS ..
    cmake --build .

    cd $TRAVIS_BUILD_DIR
}

echo
echo Build
echo

echo Task is $TASK

case $TASK in
    FAI) 
    build_FAI
    ;;
    example-blas)
    build_example_blas
    ;;
esac