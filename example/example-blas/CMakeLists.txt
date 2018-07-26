# Copyright (C) 2018 Johann Lee <me@qinka.pro>
#
# This file is part of Haskell-FAI
#
# example-blas is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# example-blas is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Haskell-FAI. If not, see <http://www.gnu.org/licenses/>.
#

# CMake 3.3
cmake_minimum_required(VERSION 3.3)

# example-blas  
project(example-blas VERSION 0.1.0.0)

## setting up the c++-11 and c-11
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_C_STANDARD   11)


# OPT FLAG
if (${ENABLE_O3})
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        # using Clang
        message("Use Clang")
        set(OPT_FLAG " -O3 ")
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        # using GCC
        message("Use GCC")
        set(OPT_FLAG " -O3 ")
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "PGI")
        # using PGI
        message("Use PGI")
        set(OPT_FLAG " -O3 ")
        if(${ACC_TARGET})
            message("Usgae target ${ACC_TARGET}")
            set(ACC_TARGET_FLAG "-ta=${ACC_TATGET}")
        endif()
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
        # using Visual Studio C++
        set(OPT_FLAG " /O3 ")
    endif()

    set(CMAKE_C_FLAGS          "${CMAKE_C_FLAGS}   ${OPT_FLAG}")
    set(CMAKE_CXX_FLAGS        "${CMAKE_CXX_FLAGS} ${OPT_FLAG}")
endif()


if(${MSVC})
  set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif(${MSVC})

## Accelerate Backend
# OpenMP
option(ENABLE_OPENMP         "Enable OpenMP"                       Off)
option(ENABLE_OPENMP_TARGET  "Enable OpenMP using target"          Off)
# OpenACC
option(ENABLE_OPENACC        "Enable OpenACC"                      Off)
option(ENABLE_OPENACC_DRVPTR "Enable OpenACC using device pointer" Off)
## TARGET
set(ACC_TARGET "" CACHE STRING "The accelerate target platform")

## Can not enable OpenMP and OpenAcc at the same time.
if(${ENABLE_OPENMP} AND ${ENABLE_OPENACC})
    message(FATAL_ERROR "Can not enable OpenMP and OpenAcc at the same time!")
endif()

## OpenMP
if(ENABLE_OPENMP)
    FIND_PACKAGE(OpenMP REQUIRED)
    if(OPENMP_FOUND)
        message("OPENMP FOUND")
        set(CMAKE_C_FLAGS          "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS} ${ACC_TARGET_FLAG}")
        set(CMAKE_CXX_FLAGS        "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS} ${ACC_TARGET_FLAG}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
        message("Enable OpenMP")
        add_definitions(-D_OMP_ENABLE_)
        if(ENABLE_OPENMP_TARGET)
	        set(CMAKE_C_FLAGS      "${CMAKE_C_FLAGS} -omptargets=${OPENMP_TARGET}")
            add_definitions(-D_OMP_TARGET_=${ACC_TARGET})
            message("Using target(${ACC_TARGET})")
        endif()
    endif()
endif()

## OpenACC
if(ENABLE_OPENACC)
    find_package(OpenACC REQUIRED)
    if(${OpenACC_C_FOUND} AND ${OpenACC_CXX_FOUND})
        message("OpenACC Found")
        set(CMAKE_C_FLAGS          "${CMAKE_C_FLAGS} ${OpenACC_C_FLAGS}")
        set(CMAKE_CXX_FLAGS        "${CMAKE_CXX_FLAGS} ${OpenACC_CXX_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
        add_definitions(-D_OACC_ENABLE_)
        message("Enable OpenACC")
        if(ENABLE_OPENACC_DRVPTR)
            message("Using drive pointer with OpenACC")
            add_definitions(-D_OACC_DRVPTR_)
        endif()
    endif()
endif()

aux_source_directory("${PROJECT_SOURCE_DIR}/source" SRC_FILES)

include_directories("${PROJECT_SOURCE_DIR}/include")


add_library(example-blas SHARED ${SRC_FILES})




### Testing
option(BUILD_TESTS "Enable test" Off)

if(${BUILD_TESTS})
    message("Enable testing")
    enable_testing()
    find_package(GTest REQUIRED)
    include_directories(${GTEST_INCLUDE_DIRS})
    aux_source_directory("${PROJECT_SOURCE_DIR}/test" TEST_FILES)
    add_executable(example-blas-test ${TEST_FILES})
    target_link_libraries(example-blas-test ${GTEST_BOTH_LIBRARIES} example-blas)
    if(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
        target_link_libraries(example-blas-test pthread)
    endif()
    add_test(BLAS_TEST example-blas-test)
endif()
