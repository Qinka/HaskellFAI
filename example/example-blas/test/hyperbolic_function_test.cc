#include <test_prelude.h>
#include <example-blas/hyperbolic_function.h>

#if DO_VECTOR_SINH == 1
/**********************************************************************
 * 
 *  vectorsinh  forward test.
 * 
**********************************************************************/
TEST(forward_N_sinh, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = sinhf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_sinh, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_sinh(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_sinh, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_sinh(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(B[i], sinhf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector sinh backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_sinh_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_sinh_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(dA[i], dB[i] * coshf(A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_SINH == 1

#if DO_VECTOR_COSH == 1
/**********************************************************************
 * 
 *  vector cosh forward test.
 * 
**********************************************************************/
TEST(forward_N_cosh, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = coshf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_cosh, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_cosh(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_cosh, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_cosh(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(B[i], coshf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector cosh backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_cosh_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_cosh_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(dA[i], dB[i] * sinhf(A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_COSH == 1

#if DO_VECTOR_TANH == 1
/**********************************************************************
 * 
 *  vector tanh forward test.
 * 
**********************************************************************/
TEST(forward_N_tanh, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = tanhf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_tanh, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_tanh(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_tanh, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_tanh(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(B[i], tanhf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector tanh backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_tanh_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float  *B = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    for(int i = 0; i < m * n; i++)
        B[i] = tanhf(A[i]);

    backward_N_tanh_A(dA, dB, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(dA[i], dB[i] * (1 - tanhf(A[i]) * tanhf(A[i])));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_TANH == 1

///////////////////////////////////////////////////////////////////////////////////////////////////

#if DO_VECTOR_ATANH == 1
/**********************************************************************
 * 
 *  vector cosh forward test.
 * 
**********************************************************************/
TEST(forward_N_atanh, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = atanhf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_atanh, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_atanh(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_atanh, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_atanh(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(B[i], atanhf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector atanh backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_atanh_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_atanh_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(dA[i], dB[i] / (1 - A[i] * A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ATANH == 1

#if DO_VECTOR_ASINH == 1
/**********************************************************************
 * 
 *  vector asinh forward test.
 * 
**********************************************************************/
TEST(forward_N_asinh, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = asinhf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_asinh, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_asinh(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_asinh, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_asinh(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(B[i], asinhf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector asinh backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_asinh_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_asinh_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(dA[i], dB[i] / sqrt(1 + A[i] * A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ASINH == 1

#if DO_VECTOR_ACOSH == 1
/**********************************************************************
 * 
 *  vector acosh forward test.
 * 
**********************************************************************/
TEST(forward_N_acosh, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = acoshf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_acosh, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_acosh(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_acosh, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);
    for(int i = 0; i < m * n; ++i)
        A[i] += 9.21924;

    forward_N_acosh(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(B[i], acoshf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector acosh backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_acosh_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);
    for(int i = 0; i < m * n; ++i)
        A[i] += 9.21924;

    backward_N_acosh_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_NEAR(dA[i], dB[i] / sqrt(A[i] * A[i] - 1));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ACOSH == 1
