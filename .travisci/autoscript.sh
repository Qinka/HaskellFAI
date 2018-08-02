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

## Command
function common_init() {
    echo
    echo Pre-install
    echo

    echo Fetching the system\' name
    export OS_CORENAME=$(lsb_release -c | awk '{print $2}')
    export OS_DISTRIBUTOR=$(lsb_release -i | awk '{print $3}')
    echo Using $OS_DISTRIBUTOR $OS_CORENAME for building

    export APT=apt-get
    echo Updating $APT source
    sudo $APT update
}

## FAI
function FAI_init() {
    sudo $APT install -y ghc-$GHC_VER 

    if [ -n "$LLVM" ]; then
        echo
        echo Using llvm-$LLVM
        echo
        sudo $APT install -y lldb-$LLVM llvm-$LLVM llvm-$LLVM-dev llvm-$LLVM-runtime
    fi

    #
    # I was planning enable the CUDA test, but the Travis-CI do not support such things.
    # So it's abandoned now.
    #
    if [ -n "$CUDA" ]; then
        echo CUDA should be disable.
        exit 3
    fi

    echo Setting up ghc-$GHC_VER
    export PATH=/opt/ghc/$GHC_VER/bin:$PATH
    ghc -V

    mkdir -p $HOME/.local/bin
    export PATH=$HOME/.local/bin:$PATH
    travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'


    echo Configuring
    stack config set system-ghc --global true
}

function FAI_build() {
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
    stack build $FLAGS FAI
}

function FAI_test() {
    cd $TRAVIS_BUILD_DIR
    echo
    echo Run testing of FAI
    stack test $FLAGS FAI
}

function FAI_succ() {
    echo "Nothing to do"
}

## example-blas
function example-blas_init() {
    echo Build gtest
    cd /usr/src/gtest/
    sudo cmake -DCMAKE_INSTALL_PREFIX=/usr/ .
    sudo make
    sudo find .
}

function example-blas_build() {
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


    cmake -DBUILD_TESTS=$TEST_STATUS $ACC_CMAKE_FLAGS -DGTEST_ROOT=/usr/src/gtest ..
    cmake --build .

    cd $TRAVIS_BUILD_DIR
}

function example-blas_test() {
    echo
    echo Run testing of examples of blas
    cd $EBLAS_BUILD_DIR
    ctest -V
}

function example-blas_succ() {
    echo "Nothing to do"
}

## hExampleBLAS
function hExampleBLAS_init() {
    FAI_init
    example-blas_init
    FAI_build
    example-blas_build
}

function hExampleBLAS_build() {
    stack build $FLAGS hExampleBLAS
}

function hExampleBLAS_test() {
    echo "Nothing to do"
}

function hExampleBLAS_succ() {
    echo "Nothing to do"
}