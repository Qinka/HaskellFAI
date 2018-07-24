#pragma once
#ifndef _CONFIG_H_
#define _CONFIG_H_

// Can not enable OpenMP and OpenACC at the same time.
#if (defined _OMP_ENABLE_) && (defined _OACC_ENABLE_)
#error Can not enable _OMP_ENABLE_ and _OACC_ENABLE_ at the same time.
#endif

// Include
#ifdef _OMP_ENABLE_
#include <omp.h>
#endif

#ifdef _OACC_ENABLE_
#include <openacc.h>
#endif

// Fix
#if (defined _OMP_TARGET_) && !(defined _OMP_ENABLE_)
#define _OMP_ENABLE_
#endif

#if (defined _OACC_DRVPTR_) && !(defined _OACC_ENABLE_)
#define _OACC_ENABLE_
#endif


// Declare
#define NO_ACC        0
#define OMP_ONLY      1
#define OMP_TARGET    2
#define OACC_ONLY     3
#define OACC_DRVPTR   4
#define OMP_ENABLE    1
#define OACC_ENABLE   2

#if defined _OMP_ENABLE_
#  if defined _OMP_TARGET_
#    define ACC_REGION OMP_TARGET
#  else
#    define ACC_REGION OMP_ONLY 
#  endif
#  define ACC_LOOP OMP_ENABLE 
#elif defined _OACC_ENABLE_
#  if defined _OACC_DRVPTR_
#    define ACC_REGION OACC_DRVPTR 
#  else
#    define ACC_REGION OACC_ONLY
#  endif
#  define ACC_LOOP OACC_ENABLE
#else 
#  define ACC_REGION NO_ACC
#  define ACC_LOOP   NO_ACC
#endif

// Configures region
// #if   ACC_REGION == OMP_ONLY
//
// #elif ACC_REGION == OMP_TARGET
//
// #elif ACC_REGION == OACC_ONLY
//
// #elif ACC_REGION == OACC_DRVPTR
//
// #endif
// Configures loop
// #if   ACC_LOOP == OMP_ENABLE
//
// #elif ACC_LOOP == OACC_ENABLE
//
// #endif

#endif // !_CONFIG_H_