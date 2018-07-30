#include <vector_opt.h>
#include <config.h>
#include <math.h>
#include <seq_opt.h>

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

//abs
void forward_vector_abs(float *B, float *A, int n) {

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
            B[i] = fabsf(A[i]);
    }
}
void backward_vector_abs_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i] * sign(A[i]);
    }
}
//sign
void forward_vector_sign(float *B, float *A, int n) {

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
            B[i] = sign(A[i]);
    }
    
}
void backward_vector_sign_A(float *dA, int n) {

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dA, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dA, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = .0f;
    }
}
//exp
void forward_vector_exp(float *B, float *A, int n) {

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
            B[i] = expf(A[i]);
    }
}
void backward_vector_exp_A(float *dA, float *dB, float *B, int n) {

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
            dA[i] = dB[i] * B[i];
    }
}
//expm1
void forward_vector_expm1(float *B, float *A, int n) {

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
            B[i] = expm1f(A[i]);
    }
}
void backward_vector_expm1_A(float *dA, float *dB, float *B, int n) {

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
            dA[i] = dB[i] * (B[i] + 1);
    }
}
//log
void forward_vector_log(float *B, float *A, int n) {

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
            B[i] = logf(A[i]);
    }
}
void backward_vector_log_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i] / A[i];
    }
}
//logp1
void forward_vector_log1p(float *B, float *A, int n) {

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
            B[i] = log1pf(A[i]);
    }
}
void backward_vector_log1p_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i] / (A[i] + 1);
    }
}
//sqrt
void forward_vector_sqrt(float *B, float *A, int n) {

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
            B[i] = sqrtf(A[i]);
    }
}
void backward_vector_sqrt_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i] / 2 / sqrtf(A[i]);
    }
}
//pow
void forward_vector_pow(float *B, float *A, float x, int n) {

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(B, A, x, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(B, A, x,  n)
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
            B[i] = powf(A[i], x);
    }
}
void backward_vector_pow_A(float *dA, float *dB, float *A, float *B, float x, int n) {

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dA, dB, A, B, x, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dA, dB, A, B, x, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(dA[0:n]), copyin(dB[0:n], A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(dA[0:n], dB[0:m*n], A[0:m*n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++)
            dA[i] = dB[i] * B[i] * x / A[i];
    }
}
void backward_vector_pow_w(float *dx, float *dB, float *A, float *B, float x, int n) {
    float sum = 0;

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(dx, dB, A, B, x, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(dx, dB, A, B, x, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(sum), copyin(dB[0:n], A[0:n], B[0:n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data copyout(sum), deviceptr(dB[0:m*n], A[0:m*n], B[0:n])
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for reduction(+:sum)
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop reduction(+:sum)
        #endif
        for (i = 0; i < n; i++)
            sum += dB[i] * B[i] * logf(A[i]);
    }
    *dx = sum;
}
//sin
void forward_vector_sin(float *B, float *A, int n) {

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
void backward_vector_sin_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i] * cos(A[i]);
    }
}
//cos
void forward_vector_cos(float *B, float *A, int n) {

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
void backward_vector_cos_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = - dB[i] * sin(A[i]);
    }
}
//tan
void forward_vector_tan(float *B, float *A, int n) {

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
void backward_vector_tan_A(float *dA, float *dB, float *A, int n) {

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
            dA[i] = dB[i]  / cosf(A[i]) / cosf(A[i]);;
    }
}
