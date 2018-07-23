#include <matrix_opt.h>
#include <config.h>

void f_matrix_mul2D(float *dst, float *A, float *B, int m, int n, int s) {

    #ifdef _OMP_ENABLE_
    #pragma omp parallel shared(dst, A, B, m, n, s) //private(i, j, k)
    #endif
    #ifdef _OMP_TARGET_ENABLE_
    #pragma omp target parallel shared(dst, A, B, m, n, s) //private(i, j, k)
    #endif
    #ifdef _OACC_ENABLE_
    #pragma acc data copyout(dst[0:m*s]), copyin(A[0:m*n], B[0:n*s])
    #endif
    {
        int i, j, k;

        #ifdef _OMP_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OMP_TARGET_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OACC_ENABLE_
        #pragma acc parallel
        #endif
        for(i = 0; i < m * s; i++)
            dst[i] = 0;

        #ifdef _OMP_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OMP_TARGET_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OACC_ENABLE_
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m; i++)
            #ifdef _OACC_ENABLE_
            #pragma acc loop
            #endif
            for (j = 0; j < s; j++)
                for (k = 0; k < n; k++)
                    dst[i * s + j] += A[i * n + k] * B[k * s + j];
    }
}

void f_matrix_dot_mul2D(float *dst, float *A, float *B, int m, int n) {

    #ifdef _OMP_ENABLE_
    #pragma omp parallel shared(dst, A, B, m, n) //private(i, j, k)
    #endif
    #ifdef _OMP_TARGET_ENABLE_
    #pragma omp target parallel shared(dst, A, B, m, n) //private(i, j, k)
    #endif
    #ifdef _OACC_ENABLE_
    #pragma acc data copyout(dst[0:m*n]), copyin(A[0:m*n], B[0:m*n])
    #endif
    {
        int i;

        #ifdef _OMP_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OMP_TARGET_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OACC_ENABLE_
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m * n; i++)
            dst[i] = A[i] * B[i];
    }
}


void f_matrix_scale_mul2D(float *dst, float *A, float scale, int m, int n){

    #ifdef _OMP_ENABLE_
    #pragma omp parallel shared(dst, A, scale, m, n) //private(i, j, k)
    #endif
    #ifdef _OMP_TARGET_ENABLE_
    #pragma omp target parallel shared(dst, A, scale, m, n) //private(i, j, k)
    #endif
    #ifdef _OACC_ENABLE_
    #pragma acc data copyout(dst[0:m*n]), copyin(A[0:m*n], scale)
    #endif
    {
        int i;

        #ifdef _OMP_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OMP_TARGET_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OACC_ENABLE_
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m * n; i++)
            dst[i] = A[i] * scale;
    }
}
