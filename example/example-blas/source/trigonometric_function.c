#include <example-blas/trigonometric_function.h>
#include <config.h>
#include <math.h>

//sin
void forward_N_sin(float *B, float *A, int n) {

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
            B[i] = sinf(A[i]);
    }
}
void backward_N_sin_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i] * cosf(A[i]);
    }
}
//cos
void forward_N_cos(float *B, float *A, int n) {

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
            B[i] = cosf(A[i]);
    }
}
void backward_N_cos_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = - dB[i] * sinf(A[i]);
    }
}
//tan
void forward_N_tan(float *B, float *A, int n) {

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
            B[i] = tanf(A[i]);
    }
}
void backward_N_tan_A(float *dA, float *dB, float *B, int n) {

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
            dA[i] = dB[i] * (1 + B[i] * B[i]);
    }
}
//atan
void forward_N_atan(float *B, float *A, int n){

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
            B[i] = atanf(A[i]);
    }
}
void backward_N_atan_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = dB[i] / (1 + A[i] * A[i]);
    }
}
//asin
void forward_N_asin(float *B, float *A, int n){

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
            B[i] = asinf(A[i]);
    }
}
void backward_N_asin_A(float *dA, float *dB, float *A, int n){

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
            dA[i] =  dB[i] / sqrtf(1 - A[i] * A[i]);
    }
}
//acos
void forward_N_acos(float *B, float *A, int n){

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
            B[i] = acosf(A[i]);
    }
}
void backward_N_acos_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = - dB[i] / sqrtf(1 - A[i] * A[i]);
    }
}