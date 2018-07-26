#include <vector_opt.h>
#include <config.h>

void forward_vector_add(float *C, float *A, float *B, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(C, A, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(C, A, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(C[0:n]), copyin(A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(C[0:n], A[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            C[i] = A[i] + B[i];
    }
}
void backward_vector_add_A(float *dA, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dC, dA, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dC, dA, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dC[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dC[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dC[i];
    }
}
void backward_vector_add_B(float *dB, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dB, dC, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dB, dC, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dB[0:n]), copyin(dC[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dB[0:n], dC[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dB[i] = dC[i];
    }
}


void forward_vector_sub(float *C, float *A, float *B, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(C, A, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(C, A, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(C[0:n]), copyin(A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(C[0:n], A[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            C[i] = A[i] - B[i];
    }
}
void backward_vector_sub_A(float *dA, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dC, dA, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dC, dA, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dC[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dC[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dC[i];
    }
}
void backward_vector_sub_B(float *dB, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dB, dC, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dB, dC, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dB[0:n]), copyin(dC[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dB[0:n], dC[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dB[i] = -dC[i];
    }
}


void forward_vector_dot_prd(float *C, float *A, float *B, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(C, A, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(C, A, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(C[0:n]), copyin(A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(C[0:n], A[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            C[i] = A[i] * B[i];
    }
}
void backward_vector_dot_prd_A(float *dA, float *B, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dC, dA, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dC, dA, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dC[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dC[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dC[i] * B[i];
    }
}
void backward_vector_dot_prd_B(float *dB, float *A, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dC, dB, A, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dC, dB, A, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dB[0:n]), copyin(dC[0:n], A[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dB[0:n], dC[0:n], A[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dB[i] = dC[i] * A[i];
    }
}


void forward_vector_dot_div(float *C, float *A, float *B, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(C, A, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(C, A, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(C[0:n]), copyin(A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(C[0:n], A[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            C[i] = A[i] / B[i];
    }
}
void backward_vector_dot_div_A(float *dA, float *B, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dC, dA, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dC, dA, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dC[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dC[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dC[i] / B[i];
    }
}
void backward_vector_dot_div_B(float *dB, float *A, float *B, float *dC, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dC, dB, A, B, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dC, dB, A, B, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dB[0:n]), copyin(dC[0:n], A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dB[0:n], dC[0:n], A[0:n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dB[i] = - dC[i] * A[i] / B[i] / B[i];
    }
}


void forward_vector_scale(float *B, float *A, float s, int n) {

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(B, A, s, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(B, A, s, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(B), copyin(A[0:n])
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
            B[i] = A[i] * s;
    }
}
void backward_vector_scale_A(float *dA, float s, float *dB, int n){

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dA, s, dB, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dA, s, dB, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dB[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dB[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = s * dB[i];
    }
}
void backward_vector_scale_s(float *ds, float *A, float *dB, int n){
    float sum = 0;

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(A, ds, dB, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(A, ds, dB, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(ds[0:1]), copyin(dB[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(ds[0:1], dB[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for reduction(+:sum)
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop reduction(+:sum)
        #endif
        for (i = 0; i < n; i++)
            sum += dB[i] * A[i];
    }
    *ds = sum;
}