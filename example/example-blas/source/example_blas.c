#include <example-blas/example_blas.h>
#include <config.h>
#include <config_gen.h>

static
const char * _example_blas_ver_str = _EXAMPLE_BLAS_VER_STR;
static
const char * _example_blas_backend_str = _ACC_BACKEND_STR;

const char * example_blas_version_str() {
    return _example_blas_ver_str;
}

int example_blas_version_major() {
    return _EXAMPLE_BLAS_VER_MAJOR;
}

int example_blas_version_minor() {
    return _EXAMPLE_BLAS_VER_MINOR;
}

int example_blas_version_patch() {
    return _EXAMPLE_BLAS_VER_PATCH;
}

int example_blas_version_diff() {
    return _EXAMPLE_BLAS_VER_DIFF;
}

const char * example_blas_backend_type_str() {
    return _example_blas_backend_str;
}

int example_blas_backend_type_id() {
    return _ACC_BACKEND_ID;
}