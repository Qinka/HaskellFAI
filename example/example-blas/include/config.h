#pragma once
#ifndef _CONFIG_H_
#define _CONFIG_H_

#ifdef _OMP_ENABLE_
#ifdef _OMP_TARGET_ENABLE_
#error Can not enable _OMP_ENABLE_ and _OMP_TARGET_ENABLE_ at the same time.
#endif
#ifdef _OACC_ENABLE_
#error Can not enable _OMP_ENABLE_ and _OACC_ENABLE_ at the same time.
#endif
#endif

#ifdef _OACC_ENABLE_
#ifdef _OMP_TARGET_ENABLE_
#error Can not enable _OACC_ENABLE_ and _OMP_TARGET_ENABLE_ at the same time.
#endif
#endif


#ifdef _OMP_ENABLE_
#include <omp.h>
#endif

#ifdef _OMP_TARGET_ENABLE_
#include <omp.h>
#endif

#ifdef _OACC_ENABLE_
#include <openacc.h>
#endif

#endif // !_CONFIG_H_