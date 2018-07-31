#include <example-blas/activation_function.h>
#include <config.h>
#include <math.h>

// sigmoid
void forward_N_sigmoid(float *B, float *A, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(B, A, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(B, A, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(B[0:n]), copyin(A[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(B[0:m*n], A[0:m*n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            B[i] = 1 / (1 +  exp(-A[i]));
    }
}
void backward_N_sigmoid_A(float *dA, float *dB, float *B, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dA, dB, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dA, dB, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dB[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dB[0:m*n], B[0:m*n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dB[i] * (1 - B[i]) / B[i] / B[i] / B[i];
    }
}
// ReLU
void forward_N_ReLU(float *B, float *A, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(B, A, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(B, A, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(B[0:n]), copyin(A[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(B[0:m*n], A[0:m*n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            B[i] = (A[i] >= 0) * A[i];
    }
}
void backward_N_ReLU_A(float *dA, float *dB, float *A, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dA, dB, A, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dA, dB, A, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dB[0:n], A[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dB[0:m*n], A[0:m*n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dB[i] * (A[i] >= 0);
    }
}
