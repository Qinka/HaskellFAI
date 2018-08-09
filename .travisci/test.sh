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
## Testing of FAI
##
##########################################################
function test_FAI() {
    cd $TRAVIS_BUILD_DIR
    echo
    echo Run testing of FAI
    stack test $FLAGS
}

##########################################################
##
## Testing of example of blas
##
##########################################################
function test_example_blas() {
    echo
    echo Run testing of examples of blas
    cd $EBLAS_BUILD_DIR
    ctest -V
}

if [ -n "$RUN_TEST" ]; then
    echo
    echo Run testing
    echo

    case $TASK in
        FAI)
            test_FAI
        ;;
        example-blas)
            test_example_blas
        ;;
    esac
fi