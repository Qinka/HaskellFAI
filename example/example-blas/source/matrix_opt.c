#include <matrix_opt.h>
#include <config.h>

void f_matrix_mul2D(float *dst, float *A, float *B, int m, int n, int s) {

    #ifdef _OMP_ENABLE_
    #pragma omp parallel shared(dst, A, B, m, n, s) private(i, j, k)
    #endif
    #ifdef _OMP_TARGET_ENABLE_
    #pragma omp target parallel shared(dst, A, B, m, n, s) private(i, j, k)
    #endif
    #ifdef _OACC_ENABLE_
    #pragma acc data copyin(dst[0:m*s]), copyin(A[0:m*n], B[0:n*s]) private(i, j, k)
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
        #pragma acc kernels
        #endif
        for(i = 0; i < m; i++)
            for(j = 0; j < s; j++)
                dst[i * s + j] = 0;

        #ifdef _OMP_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OMP_TARGET_ENABLE_
        #pragma omp for
        #endif
        #ifdef _OACC_ENABLE_
        #pragma acc kernels
        #endif
        for (i = 0; i < m; i++)
            for (j = 0; j < s; j++)
                #ifdef _OMP_ENABLE_
                #pragma omp seq
                #endif
                #ifdef _OMP_TARGET_ENABLE_
                #pragma omp seq
                #endif
                #ifdef _OACC_ENABLE_
                #pragma acc seq
                #endif
                for (k = 0; k < n; k++)
                    dst[i * s + j] += A[i * n + k] * B[k * s + j];
    }
}

void f_matrix_dot_mul2D(float *dst, float *A, float *B, int m, int n) {
    int i;

    #ifdef _OMP_ENABLE_
    #pragma omp parallel for shared(dst, A, B, m, n) private(i)
    #endif
    #ifdef _OMP_TARGET_ENABLE_
    #pragma omp target parallel for shared(dst, A, B, m, n) private(i)
    #endif
    for (i = 0; i < m * n; i++)
        dst[i] = A[i] * B[i];
}


void f_matrix_scale_mul2D(float *dst, float *A, float scale, int m, int n){
    int i;
    
    #ifdef _OMP_ENABLE_
    #pragma omp parallel for shared(dst, A, m, n) private(i)
    #endif
    #ifdef _OMP_TARGET_ENABLE_
    #pragma omp target parallel for shared(dst, A, m, n) private(i)
    #endif
    for (i = 0; i < m * n; i++)
        dst[i] = A[i] * scale;    
}
