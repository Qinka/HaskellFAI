#include <matrix_opt.h>
#include <config.h>

void forward_matrix_mul2D(float *C, float *A, float *B, int m, int n, int s) {
    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(C, A, B, m, n, s)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(C, A, B, m, n, s)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(C[0:m*s]), copyin(A[0:m*n], B[0:n*s])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(C[0:m*s], A[0:m*n], B[0:n*s])
    #endif
    {
        int i, j, k;
        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for(i = 0; i < m * s; i++)
            C[i] = 0;

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
                    C[i * s + j] += A[i * n + k] * B[k * s + j];
    }
}
void backward_matrix_mul2D_A(float *dA, float *dC, float *B, int m, int n, int s) {
    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dA, B, dC, m, n, s)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dA, B, dC, m, n, s)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:m*n]), copyin(B[0:n*s], dC[0:m*s])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:m*n], B[0:n*s], dC[0:m*s])
    #endif
    {
        int i, j, k;
        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for(i = 0; i < m * n; i++)
            dA[i] = 0;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < m; i++)
            #if ACC_LOOP == OACC_ENABLE
            #pragma acc loop
            #endif
            for (j = 0; j < n; j++)
                for (k = 0; k < s; k++)
                    dA[i * n + j] += dC[i * s + k] * B[j * s + k]; // B^T
    }
}
void backward_matrix_mul2D_B(float *dB, float *A, float *dC, int m, int n, int s){
    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dB, A, dC, m, n, s)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dB, A, dC, m, n, s)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dB[0:n*s]), copyin(A[0:m*n], dC[0:m*s])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dB[0:n*s], A[0:m*n], dC[0:m*s])
    #endif
    {
        int i, j, k;
        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for(i = 0; i < m * n; i++)
            dB[i] = 0;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            #if ACC_LOOP == OACC_ENABLE
            #pragma acc loop
            #endif
            for (j = 0; j < s; j++)
                for (k = 0; k < m; k++)
                    dB[i * s + j] += A[k * n + i] * dC[k * s + j]; // A^T
    }
}
