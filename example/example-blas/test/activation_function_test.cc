#include <test_prelude.h>
#include <example-blas/activation_function.h>

#if DO_VECTOR_SIGMOID == 1
/**********************************************************************
 * 
 *  vector sigmoid forward test.
 * 
**********************************************************************/
TEST(forward_N_sigmoid, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = 1 / (1 + expf(-A[i]));

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_sigmoid, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_sigmoid(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_sigmoid, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_sigmoid(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], 1 / (1 + expf(-A[i])));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector sigmoid backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_sigmoid_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float  *B = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    for(int i = 0; i < m * n; i++)
        B[i] = 1 / (1 + expf(-A[i]));

    backward_N_sigmoid_A(dA, dB, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * ( (1 - B[i]) / B[i]  / B[i]  / B[i] ));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_SIGMOID == 1

#if DO_VECTOR_RELU == 1
/**********************************************************************
 * 
 *  vector ReLU forward test.
 * 
**********************************************************************/
TEST(forward_N_ReLU, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = (A[i] >= 0) * A[i];

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_ReLU, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_N_ReLU(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_N_ReLU, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_N_ReLU(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], (A[i] >= 0) * A[i]);
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector ReLU backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_N_ReLU_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_N_ReLU_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * (A[i] >= 0));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_RELU == 1