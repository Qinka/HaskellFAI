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

if(${MSVC})
  set (CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
endif(${MSVC})

## Accelerate Backend
set(ENABLE_OPENMP On CACHE BOOL "Enable OpenMP")
set(ENABLE_OPENMP_CLANG_CUDA Off CACHE BOOL "Enabel OpenMP with Clang for Nvidia GPU")
set(ENABLE_OPENACC Off CACHE BOOL "Enable OpenACC")

## Can not enable OpenMP and OpenAcc at the same time.
if(${ENABLE_OPENMP} AND ${ENABLE_OPENACC})
    message(FATAL_ERROR "Can not enable OpenMP and OpenAcc at the same time!")
endif()

## OpenMP
if(ENABLE_OPENMP)
    FIND_PACKAGE(OpenMP REQUIRED)
    if(OPENMP_FOUND)
        message("OPENMP FOUND")
        set(CMAKE_C_FLAGS          "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
        set(CMAKE_CXX_FLAGS        "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
        if(ENABLE_OPENMP_CLANG_CUDA)
            add_definitions(-D_OMP_CUDA_ENABLE_)
            message("Enable OpenMP with CUDA")
        else()
            add_definitions(-D_OMP_ENABLE_)
            message("Enable OpenMP")
        endif()
    endif()
endif()

## OpenACC
if(ENABLE_OPENACC)
    find_package(OpenAcc REQUIRED)
    if(${OpenACC_C_FOUND} AND ${OpenACC_CXX_FOUND})
        message("OpenAcc Found")
        set(CMAKE_C_FLAGS          "${CMAKE_C_FLAGS} ${OpenACC_C_FLAGS}")
        set(CMAKE_CXX_FLAGS        "${CMAKE_CXX_FLAGS} ${OpenACC_CXX_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
        add_definitions(-D_OACC_ENABLE_)
        message("Enable OpenACC")
    endif()
endif()

aux_source_directory("${PROJECT_SOURCE_DIR}/source" SRC_FILES)

include_directories("${PROJECT_SOURCE_DIR}/include")


add_library(example-blas SHARED ${SRC_FILES})



### Testing
enable_testing()

## GoogleTest

set(DOWNLOAD_GTEST Off CACHE BOOL "Download gtest")
if(${DOWNLOAD_GTEST})
    configure_file(gtest.cmake.in googletest-download/CMakeLists.txt)
    execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
        RESULT_VARIABLE result
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/googletest-download )
    if(result)
        message(FATAL_ERROR "CMake step for googletest failed: ${result}")
    endif()
    execute_process(COMMAND ${CMAKE_COMMAND} --build .
        RESULT_VARIABLE result
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/googletest-download )
    if(result)
        message(FATAL_ERROR "Build step for googletest failed: ${result}")
    endif()
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    add_subdirectory(
        ${CMAKE_BINARY_DIR}/googletest-src
        ${CMAKE_BINARY_DIR}/googletest-build
        EXCLUDE_FROM_ALL
        )
else()
    find_package(GTest REQUIRED)
endif()

include_directories(${GTEST_INCLUDE_DIRS})
# aux_source_directory("${PROJECT_SOURCE_DIR}/test" TEST_FILES)
add_executable(example-blas-test ${PROJECT_SOURCE_DIR}/test/test.cc)
target_link_libraries(example-blas-test ${GTEST_BOTH_LIBRARIES} example-blas)
if(NOT(CMAKE_SYSTEM_NAME MATCHES "Windows"))
    target_link_libraries(example-blas-test pthread)
endif()
add_test(BLAS_TEST example-blas-test)