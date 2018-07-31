#include <example-blas/hyperbolic_function.h>
#include <config.h>
#include <math.h>

void forward_N_cosh(float *B, float *A, int n){

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
            B[i] = coshf(A[i]);
    }
}
void backward_N_cosh_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = dB[i] * sinhf(A[i]);
    }
}
//sinh
void forward_N_sinh(float *B, float *A, int n){

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
            B[i] = sinhf(A[i]);
    }
}
void backward_N_sinh_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = dB[i] * coshf(A[i]);
    }
}
//tanh
void forward_N_tanh(float *B, float *A, int n){

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
            B[i] = tanhf(A[i]);
    }
}
void backward_N_tanh_A(float *dA, float *dB, float *B, int n){

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
            dA[i] = dB[i] * (1 - B[i] * B[i]);
    }
}
//atanh
void forward_N_atanh(float *B, float *A, int n){

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
            B[i] = atanhf(A[i]);
    }
}
void backward_N_atanh_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = dB[i] / (1 - A[i] * A[i]);
    }
}
//acosh
void forward_N_acosh(float *B, float *A, int n){

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
            B[i] = acoshf(A[i]);
    }
}
void backward_N_acosh_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = dB[i] / sqrtf(A[i] * A[i] - 1);
    }
}
//asinh
void forward_N_asinh(float *B, float *A, int n){

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
            B[i] = asinhf(A[i]);
    }
}
void backward_N_asinh_A(float *dA, float *dB, float *A, int n){

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
            dA[i] = dB[i] / sqrtf(A[i] * A[i] + 1);
    }
}