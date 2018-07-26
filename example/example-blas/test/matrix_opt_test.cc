#include "test_prelude.h"
#include <matrix_opt.h>

#if DO_MATRIX_PRODUCT == 1
/**********************************************************************
 * 
 *  matrix product forward test.
 * 
**********************************************************************/
TEST(forward_matrix_multiplication, no_parallel_case0) {    
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

TEST(forward_matrix_multiplication, parallel_case1) {    
    int m = 512, n = 512, s = 512;
    float *dst = new float[m * s];
    float *A   = new float[m * n];
    float *B   = new float[n * s];

    forward_matrix_mul2D(dst, A, B, m, n, s);

    EXPECT_EQ(1,1);
    delete[] dst;
    delete[] A;
    delete[] B;
}

TEST(forward_matrix_multiplication, eye_case2) {
    const int m = 4, s = 4;
    float *eye = new float[m * m];
    float *ra  = new float[m * s];
    float *rt  = new float[m * s];

    fill_eye(eye, m);
    fill_random(ra, m * s);

    forward_matrix_mul2D(rt, eye, ra, m, m, s);

    for(int i = 0; i < m * s; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] eye;
}

TEST(forward_matrix_multiplication, eye_case3) {
    const int m = 4, s = 4;
    float *eye = new float[s * s];
    float *ra  = new float[m * s];
    float *rt  = new float[m * s];

    fill_eye(eye, m);
    fill_random(ra, m * s);

    forward_matrix_mul2D(rt, ra, eye, m, m, s);

    for(int i = 0; i < m * s; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] eye;
}

/**********************************************************************
 * 
 *  matrix product backward test. (for matrix A)
 * 
**********************************************************************/
TEST(backward_matrix_multiplication_A, specific_case0){
    const int m = 4, n = 4, s = 4;
    float *dA    =  new float[m * n];
    float  dC[]  =  {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float   B[]  =  { 0.7519, 0.9108, 1.3465, 1.2250
                    , 0.0537, -0.4415, -1.0207,  0.0134
                    , -0.6060, -0.7769, -0.3740, -0.3617
                    , 1.8522, -1.0480, -1.4334, -0.1984};
    float dARt[] =  { 4.2342, -1.3952, -2.1187, -0.8276
                    , 4.2342, -1.3952, -2.1187 ,-0.8276
                    , 4.2342, -1.3952, -2.1187 ,-0.8276
                    , 4.2342, -1.3952, -2.1187 ,-0.8276};

    backward_matrix_mul2D_A(dA, dC, B, m, n, s);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dA[i],dARt[i],0.0002);
    }

    delete[] dA;
}
TEST(backward_matrix_multiplication_A, specific_case1){
    const int m = 4, n = 4, s = 4;
    float *dA    =  new float[m * n];
    float  dC[]  =  {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float   B[]  =  { 0.9872, -0.1312,  2.3800, -1.6548
                    , -0.3610,  1.7343,  0.4045, -0.3674
                    , -0.8110,  2.0987,  0.7145, -0.9011
                    , -2.6406, -0.7345,  0.9213, -1.2033};
    float dARt[] =  { 1.5811,  1.4104,  1.1010, -3.6571
                    , 1.5811,  1.4104,  1.1010, -3.6571
                    , 1.5811,  1.4104,  1.1010, -3.6571
                    , 1.5811,  1.4104,  1.1010, -3.6571};

    backward_matrix_mul2D_A(dA, dC, B, m, n, s);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dA[i],dARt[i],0.0002);
    }

    delete[] dA;
}

/**********************************************************************
 * 
 *  matrix product backward test. (for matrix B)
 * 
**********************************************************************/
TEST(backward_matrix_multiplication_B, specific_case0){
    const int m = 4, n = 4, s = 4;
    float *dB    =  new float[m * n];
    float  dC[]  =  {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float   A[]  =  { 1,  0,  0,  0
                    , 0,  1,  0,  0
                    , 0,  0,  1,  0
                    , 0,  0,  0,  1};
    float dBRt[] =  {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};

    backward_matrix_mul2D_B(dB, A, dC, m, n, s);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dB[i],dBRt[i],0.0002);
    }

    delete[] dB;
}
TEST(backward_matrix_multiplication_B, specific_case1){
    const int m = 4, n = 4, s = 4;
    float *dB    =  new float[m * n];
    float  dC[]  =  {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float   A[]  =  { 0.4724, -1.1202, -0.3951, -0.3763
                    , 0.9235,  0.4414, -0.1743, -0.5109
                    , -0.4045, -0.5956, -1.7240,  0.0521
                    , 2.4032, -0.3303,  0.4600, -1.3693};
    float dBRt[] =  { 3.3946,  3.3946,  3.3946,  3.3946
                    , -1.6047, -1.6047, -1.6047, -1.6047
                    , -1.8335, -1.8335, -1.8335, -1.8335
                    , -2.2044, -2.2044, -2.2044, -2.2044};

    backward_matrix_mul2D_B(dB, A, dC, m, n, s);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dB[i],dBRt[i],0.0002);
    }

    delete[] dB;
}
#endif // DO_MATRIX_PRODUCT == 1