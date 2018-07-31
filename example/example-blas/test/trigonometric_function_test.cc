#include <test_prelude.h>
#include <example-blas/trigonometric_function.h>


#if DO_VECTOR_SIN == 1
/**********************************************************************
 * 
 *  vectorsin  forward test.
 * 
**********************************************************************/
TEST(forward_N_sin, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = sinf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_sin, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_sin(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_sin, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_sin(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], sinf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector sin backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_sin_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_sin_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * cosf(A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_SIN == 1

#if DO_VECTOR_COS == 1
/**********************************************************************
 * 
 *  vector cos forward test.
 * 
**********************************************************************/
TEST(forward_N_cos, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = cosf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_cos, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_cos(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_cos, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_cos(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], cosf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector cos backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_cos_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_cos_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * (- sinf(A[i])));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_COS == 1

#if DO_VECTOR_TAN == 1
/**********************************************************************
 * 
 *  vector tan forward test.
 * 
**********************************************************************/
TEST(forward_N_tan, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = tanf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_tan, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_tan(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_tan, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_tan(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], tanf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector tan backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_tan_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float  *B = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    for(int i = 0; i < m * n; i++)
        B[i] = tanf(A[i]);

    backward_N_tan_A(dA, dB, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * (1 + tanf(A[i]) * tanf(A[i])));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_TAN == 1


#if DO_VECTOR_ATAN == 1
/**********************************************************************
 * 
 *  vector cos forward test.
 * 
**********************************************************************/
TEST(forward_N_atan, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = atanf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_atan, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_atan(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_atan, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_atan(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], atanf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector atan backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_atan_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_atan_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] / (1 +  powf(A[i],2)));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ATAN == 1

#if DO_VECTOR_ASIN == 1
/**********************************************************************
 * 
 *  vector asin forward test.
 * 
**********************************************************************/
TEST(forward_N_asin, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = asinf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_asin, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_asin(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_asin, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_asin(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], asinf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector asin backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_asin_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_asin_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] / sqrtf(1 - A[i] * A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ASIN == 1

#if DO_VECTOR_ACOS == 1
/**********************************************************************
 * 
 *  vector acos forward test.
 * 
**********************************************************************/
TEST(forward_N_acos, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = acosf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_acos, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_acos(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_acos, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_acos(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], acosf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector acos backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_acos_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_acos_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], - dB[i] / sqrtf(1 - A[i] * A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ACOS == 1