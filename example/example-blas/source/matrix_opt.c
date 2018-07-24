#include <matrix_opt.h>
#include <config.h>

void f_matrix_mul2D(float *dst, float *A, float *B, int m, int n, int s) {

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dst, A, B, m, n, s)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dst, A, B, m, n, s)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dst[0:m*s]), copyin(A[0:m*n], B[0:n*s])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data copyout(dst[0:m*s]), copyin(A[0:m*n], B[0:n*s])
    #endif
    {
        int i, j, k;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for(i = 0; i < m * s; i++)
            dst[i] = 0;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m; i++)
            #if ACC_LOOP == OACC_ENABLE
            #pragma acc loop
            #endif
            for (j = 0; j < s; j++)
                for (k = 0; k < n; k++)
                    dst[i * s + j] += A[i * n + k] * B[k * s + j];
    }
}

void f_matrix_dot_mul2D(float *dst, float *A, float *B, int m, int n) {

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dst, A, B, m, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dst, A, B, m, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dst[0:m*n]), copyin(A[0:m*n], B[0:m*n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data copyout(dst[0:m*n]), copyin(A[0:m*n], B[0:m*n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m * n; i++)
            dst[i] = A[i] * B[i];
    }
}


void f_matrix_scale_mul2D(float *dst, float *A, float scale, int m, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dst, A, scale, m, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dst, A, scale, m, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dst[0:m*n]), copyin(A[0:m*n], scale)
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data copyout(dst[0:m*n]), copyin(A[0:m*n], scale)
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m * n; i++)
            dst[i] = A[i] * scale;
    }
}
