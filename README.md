# HaskellFAI
Haskell Foreign Accelerate Interface

[![Hackage](https://img.shields.io/hackage/v/FAI.svg)](https://hackage.haskell.org/package/FAI)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/FAI.svg)
[![license](https://img.shields.io/github/license/qinka/HaskellFAI.svg)](https://github.com/Qinka/HaskellFAI/blob/master/LICENSE)
[![GitHub forks](https://img.shields.io/github/forks/qinka/HaskellFAI.svg?style=social&label=Fork)](https://github.com/qinka/HaskellFAI/fork)
[![GitHub stars](https://img.shields.io/github/stars/qinka/HaskellFAI.svg?style=social&label=Stars)](https://github.com/qinka/HaskellFAI/stars)
![Github search hit counter](https://img.shields.io/github/search/qinka/HaskellFAI/goto.svg)

## CI state

| Branch     | status                                                                                                                       |
|---------|------------------------------------------------------------------------------------------------------------------------------|
| master     | [![Build Status](https://travis-ci.org/Qinka/HaskellFAI.svg?branch=master)](https://travis-ci.org/Qinka/HaskellFAI)    |
| dev/master | [![Build Status](https://travis-ci.org/Qinka/HaskellFAI.svg?branch=dev/master)](https://travis-ci.org/Qinka/HaskellFAI)    ||

## About

I create the Haskell FAI, and it's designed as an interface of foreign accelerate framework, such as OpenMP, CUDA. There are a basic type class which is a framework of "foreign pointer". 

The `data Buffer p a` defines a data structure where store the foreign pointers. `class FAI p` defines three platform-concerned functions:

1. `faiMemAllocate`
1. `faiMemRelease`
1. `faiMemReleaseP`

`faiMemAllocate` allocates memory on the specific platform, while
`faiMemReleaseP` provides the function pointer of releasing.
The buffer can be allocate by manuel and automatically release by runtime.

There is also a type `newtype Accelerate p a` which can specific platform.

The `FAI/src/Foreign/FAI/Platform/Host.hs` and `FAI/src/Foreign/FAI/Platform/CUDA.hs` are the example instance of host and CUDA.

## Sites

The codes are hosted on [Github](https://github.com/Qinka/HaskellFAI).

The package is available on [Hacklage](https://hackage.haskell.org/package/FAI): https://hackage.haskell.org/package/FAI.

## Build

For FAI, Cabal-1.21 or higher is needed. If you need not install CUDA SDK, you need to disable the flag `enable-cuda`.  For stack, you can add `--flag FAI:-enable-cuda`, and for cabal, you can add `--flags=-enable-cuda`. FAI is base on library cudart, and you need to make sure that it is available.

## Portable

I do believe FAI is portable to other platforms, such as Raspberry Pi. However nobody tested it as I know.

## Install

You can read [INSTALL](Install.md) for the information about how to install.

## Changelog

You can find the changelog in (pre-)releases' notes.

