#pragma once

#ifndef _EXAMPLE_BLAS_H_
#define _EXAMPLE_BLAS_H_


#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

const char * example_blas_version_str();
int example_blas_version_major();
int example_blas_version_minor();
int example_blas_version_patch();
int example_blas_version_diff();

const char * example_blas_backend_type_str();
int example_blas_backend_type_id();

#ifdef __cplusplus
}
#endif // ! __cplusplus


#endif // ! _EXAMPLE_BLAS_H_