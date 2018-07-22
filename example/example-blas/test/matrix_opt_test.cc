#include <gtest/gtest.h>
#include <matrix_opt.h>
#include <stdint.h>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <cstdarg>
#include <functional>
#include <random>

void fill_eye(float * mat, int row_num) {
    for(int i = 0; i < row_num; i++)
        for(int j = 0; j < row_num; j++)
            mat[i * row_num + j] = (i == j) ? 1. : 0.;
}

void fill_zero(float *mat, int num) {
    for(int i = 0; i < num; i++)
        mat[i] = 0.;
}

void fill_random(float *mat, int num) {
    std::default_random_engine generator;
    std::uniform_real_distribution<float> distribution(0,1);
    for(int i = 0; i < num; i++)
        mat[i] = distribution(generator);
}

void copy(float *dstMat, float *srcMat, int num) {
    for(int i = 0; i < num; i++)
        dstMat[i] = srcMat[i];
}

void fill_with(float *mat, float f, int num) {
    for(int i = 0; i < num; i++)
        mat[i] = f;
}


////////////////////////////////////////////////////////////mat mul
TEST(matrix_multiplication, no_parallel_case0) {    
    int m = 512, n = 512, s = 512;
    float *dst = new float[m * s];
    float *A   = new float[m * n];
    float *B   = new float[n * s];

    int i, j ,k;
    for (i = 0; i < m; i++)
        for (j = 0; j < s; j++) {
            dst[i * s + j] = 0;
            for (k = 0; k < n; k++)
                dst[i * s + j] += A[i * n + k] * B[k * s + j];
        }

    EXPECT_EQ(1,1);
    delete[] dst;
    delete[] A;
    delete[] B;
}

TEST(matrix_multiplication, parallel_case1) {    
    int m = 512, n = 512, s = 512;
    float *dst = new float[m * s];
    float *A   = new float[m * n];
    float *B   = new float[n * s];

    f_matrix_mul2D(dst, A, B, m, n, s);

    EXPECT_EQ(1,1);
    delete[] dst;
    delete[] A;
    delete[] B;
}

TEST(matrix_multiplication, eye_case2) {
    const int m = 4, s = 4;
    float *eye = new float[m * m];
    float *ra  = new float[m * s];
    float *rt  = new float[m * s];

    fill_eye(eye, m);
    fill_random(ra, m * s);

    f_matrix_mul2D(rt, eye, ra, m, m, s);

    for(int i = 0; i < m * s; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] eye;
}

TEST(matrix_multiplication, eye_case3) {
    const int m = 4, s = 4;
    float *eye = new float[s * s];
    float *ra  = new float[m * s];
    float *rt  = new float[m * s];

    fill_eye(eye, m);
    fill_random(ra, m * s);

    f_matrix_mul2D(rt, ra, eye, m, m, s);

    for(int i = 0; i < m * s; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] eye;
}

///////////////////////////////////////////////////////////// mat dot mul

TEST(matrix_dot_multiplication, no_parallel_case0) {
    int m = 4096, n = 4096;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    float *matC = new float[m * n];

    for (int i = 0; i < m; i++)
        for(int j = 0; j < n; j++)
            matC[i * n + j] = matA[i * n + j] * matB[i * n + j];
    
    EXPECT_EQ(1,1);

    delete[] matA;
    delete[] matB;
    delete[] matC;
}

TEST(matrix_dot_multiplication, parallel_case1) {
    int m = 4096, n = 4096;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    float *matC = new float[m * n];

    f_matrix_dot_mul2D(matC, matA, matB, m, n);

    EXPECT_EQ(1,1);

    delete[] matA;
    delete[] matB;
    delete[] matC;
}


TEST(matrix_dot_multiplication, eye_case2) {
    const int m = 4, n = 4;
    float *ones = new float[m * n];
    float *ra   = new float[m * n];
    float *rt   = new float[m * n];

    fill_with(ones, 1, m * n);
    fill_random(ra, m * n);

    f_matrix_dot_mul2D(rt, ra, ones, m, n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] ones;
}

TEST(matrix_dot_multiplication, eye_case3) {
    const int m = 4, n = 4;
    float *ones = new float[m * n];
    float *ra   = new float[m * n];
    float *rt   = new float[m * n];

    fill_with(ones, 1, m * n);
    fill_random(ra, m * n);

    f_matrix_dot_mul2D(rt, ones, ra, m, n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] ones;
}

TEST(matrix_scale_mul, no_parallel_case0) {
    int m = 4096, n = 4096;
    float *matA = new float[m * n];
    float *matB = new float[m * n];

    for (int i = 0; i < m; i++)
        for(int j = 0; j < n; j++)
            matB[i * n + j] = matA[i * n + j] * 2;

    EXPECT_EQ(1,1);

    delete[] matA;
    delete[] matB;
}

TEST(matrix_scale_mul, parallel_case1) {
    int m = 4096, n = 4096;
    float *matA = new float[m * n];
    float *matB = new float[m * n];

    f_matrix_scale_mul2D(matB,matA,2,m,n);

    EXPECT_EQ(1,1);

    delete[] matA;
    delete[] matB;
}

TEST(matrix_scale_mul, s1_case2) {
    int m = 16, n = 16;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    fill_random(matA, m * n);

    f_matrix_scale_mul2D(matB,matA,1,m,n);

    for(int i = 0; i < m * n; i++) {
        EXPECT_EQ(matA[i], matB[i]);
    }

    delete[] matA;
    delete[] matB;
}

TEST(matrix_scale_mul, s0_case3) {
    int m = 16, n = 16;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    fill_random(matA, m * n);

    f_matrix_scale_mul2D(matB,matA,0,m,n);

    for(int i = 0; i < m * n; i++) {
        EXPECT_EQ(0, matB[i]);
    }

    delete[] matA;
    delete[] matB;
}
