#include <matrix_opt.h>
#include <omp.h>


void f_matrix_mul2D(float *dst, float *A, float *B, int m, int n, int s) {
    int i, j;
    #pragma omp parallel shared(dst, A, B, m, n, s) private(i, j)
    {
        #pragma omp for
        for (i = 0; i < m; i++)
            for (j = 0; j < s; j++) {
                dst[i * s + j] = 0.0;
                for (int k = 0; k < n; k++)
                    dst[i * s + j] += A[i * n + k] * B[k * s + j];
            }
    }
}

void f_matrix_dot_mul2D(float *dst, float *A, float *B, int m, int n) {
    int i;
    #pragma omp parallel shared(dst, A, B, m, n) private(i)
    {
            #pragma omp for schedule(runtime)
            for (i = 0; i < m * n; i++)
                    dst[i] = A[i] * B[i];
    }
}


void f_matrix_scale_mul2D(float *dst, float *A, float scale, int m, int n){
    int i;
    #pragma omp parallel shared(dst, A, m, n) private(i)
    {
            #pragma omp for schedule(runtime)
            for (i = 0; i < m * n; i++)
                    dst[i] = A[i] * scale;
    }    
}