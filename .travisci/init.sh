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

set -e


##########################################################
##
## Initialization of FAI
##
##########################################################
function init_FAI() {
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

##########################################################
##
## Initialization of example of blas
##
##########################################################
function init_example_blas() {
    echo Build gtest
    cd /usr/src/gtest/
    sudo cmake -DCMAKE_INSTALL_PREFIX=/usr/ .
    sudo make
    sudo find .
}

##########################################################
##
## Common initialization
##
##########################################################
function init_common() {
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


init_common
case $TASK
    FAI)
        init_FAI
    ;;
    example-blas)
        init_example_blas
    ;;
esac

cd $TRAVIS_BUILD_DIR