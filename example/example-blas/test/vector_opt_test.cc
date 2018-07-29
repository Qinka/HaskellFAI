#include "test_prelude.h"
#include <vector_opt.h>

#if DO_VECTOR_ADD == 1
/**********************************************************************
 * 
 *  vector add forward test.
 * 
**********************************************************************/
TEST(forward_vector_add, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        C[i] = A[i] + B[i];

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
    delete[] C;
}
TEST(forward_vector_add, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    forward_vector_add(C, A, B, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
    delete[] C;
}
TEST(forward_vector_add, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    fill_random(A, m * n);
    fill_random(B, m * n);

    forward_vector_add(C, A, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(C[i], A[i] + B[i]);
    }

    delete[] A;
    delete[] B;
    delete[] C;
}
/**********************************************************************
 * 
 *  vector add backward test. (matrix A)
 * 
**********************************************************************/
TEST(backward_vector_add_A, random_case0) {
    const int m = 16, n = 16;
    float *dC = new float[m * n];
    float *d = new float[m * n];

    fill_random(dC, m * n);

    backward_vector_add_A(d, dC, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(d[i], dC[i]);
    }

    delete[] d;
    delete[] dC;
}
/**********************************************************************
 * 
 *  vector add backward test. (matrix B)
 * 
**********************************************************************/
TEST(backward_vector_add_B, random_case0) {
    const int m = 16, n = 16;
    float *dC = new float[m * n];
    float *d = new float[m * n];

    fill_random(dC, m * n);

    backward_vector_add_B(d, dC, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(d[i], dC[i]);
    }

    delete[] d;
    delete[] dC;
}
#endif // DO_VECTOR_ADD == 1

#if DO_VECTOR_SUB == 1
/**********************************************************************
 * 
 *  vector subtraction forward test.
 * 
**********************************************************************/
TEST(forward_vector_sub, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        C[i] = A[i] - B[i];

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
    delete[] C;
}
TEST(forward_vector_sub, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    forward_vector_sub(C, A, B, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
    delete[] C;
}
TEST(forward_vector_sub, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    fill_random(A, m * n);
    fill_random(B, m * n);

    forward_vector_sub(C, A, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(C[i], A[i] - B[i]);
    }

    delete[] A;
    delete[] B;
    delete[] C;
}
/**********************************************************************
 * 
 *  vector sub backward test. (matrix A)
 * 
**********************************************************************/
TEST(backward_vector_sub_A, random_case0) {
    const int m = 16, n = 16;
    float *dC = new float[m * n];
    float *d = new float[m * n];

    fill_random(dC, m * n);

    backward_vector_sub_A(d, dC, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(d[i], dC[i]);
    }

    delete[] d;
    delete[] dC;
}
/**********************************************************************
 * 
 *  vector sub backward test. (matrix B)
 * 
**********************************************************************/
TEST(backward_vector_sub_B, random_case0) {
    const int m = 16, n = 16;
    float *dC = new float[m * n];
    float *d = new float[m * n];

    fill_random(dC, m * n);

    backward_vector_sub_B(d, dC, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(d[i], -dC[i]);
    }

    delete[] d;
    delete[] dC;
}
#endif // DO_VECTOR_SUB == 1

#if DO_VECTOR_PRD == 1
/**********************************************************************
 * 
 *  vector dot product forward test.
 * 
**********************************************************************/
TEST(forward_vector_dotprd, no_paralel_case0){
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


TEST(forward_vector_dotprd, parallel_case1) {
    int m = 4096, n = 4096;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    float *matC = new float[m * n];
    
    forward_vector_dot_prd(matC, matA, matB, m * n);

    EXPECT_EQ(1,1);

    delete[] matA;
    delete[] matB;
    delete[] matC;
}


TEST(forward_vector_dotprd, eye_case2) {
    const int m = 4, n = 4;
    float *ones = new float[m * n];
    float *ra   = new float[m * n];
    float *rt   = new float[m * n];

    fill_with(ones, 1, m * n);
    fill_random(ra, m * n);

    forward_vector_dot_prd(rt, ra, ones, m * n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] ones;
}

TEST(forward_vector_dotprd, eye_case3) {
    const int m = 4, n = 4;
    float *ones = new float[m * n];
    float *ra   = new float[m * n];
    float *rt   = new float[m * n];

    fill_with(ones, 1, m * n);
    fill_random(ra, m * n);

    forward_vector_dot_prd(rt, ones, ra, m * n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_EQ(ra[i],rt[i]);
    }

    delete[] rt;
    delete[] ra;
    delete[] ones;
}
/**********************************************************************
 * 
 *  vector dot product backward test. (for matrix A)
 * 
**********************************************************************/
TEST(backward_vector_dot_prd_A, specific_case0){
    const int m = 4, n = 4;
    float *dA = new float[m * n];
    float  B[]    = { 0.7621, 0.6257, 0.8195, 0.4130
                    , 0.6573, -1.8253, 0.6445, 0.1668
                    , 0.7199, 1.2139, 0.3834, 0.6247
                    ,-0.6626, 0.3394, -1.1363, 0.0735};
    float dC[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};

    backward_vector_dot_prd_A(dA, B, dC, m * n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dA[i],B[i],0.0002);
    }

    delete[] dA;
}

TEST(backwad_vector_dot_prd_A, random_Case1) {
    const int m = 4, n = 4;
    float *dA = new float[m * n];
    float * B = new float[m * n];
    float  dC[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};

    fill_random(B, m * n);

    backward_vector_dot_prd_A(dA, B, dC, m * n);
    
    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dA[i],B[i],0.0002);
    }

    delete[] dA;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector dot product backward test. (for matrix B)
 * 
**********************************************************************/
TEST(backward_vector_dot_prd_B, specific_case0){
    const int m = 4, n = 4;
    float *dB = new float[m * n];
    float  A[]    = { 0.7621, 0.6257, 0.8195, 0.4130
                    , 0.6573, -1.8253, 0.6445, 0.1668
                    , 0.7199, 1.2139, 0.3834, 0.6247
                    ,-0.6626, 0.3394, -1.1363, 0.0735};
    float dC[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};

    backward_vector_dot_prd_B(dB, A, dC, m * n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dB[i],A[i],0.0002);
    }

    delete[] dB;
}

TEST(backwad_vector_dot_prd_B, random_Case1) {
    const int m = 4, n = 4;
    float *dB = new float[m * n];
    float * A = new float[m * n];
    float  dC[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};

    fill_random(A, m * n);

    backward_vector_dot_prd_B(dB, A, dC, m * n);
    
    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dB[i],A[i],0.0002);
    }

    delete[] dB;
    delete[] A;
}
#endif // DO_VECTOR_PRD == 1

#if DO_VECTOR_DOT_DIV == 1
/**********************************************************************
 * 
 *  vector dot-division forward test.
 * 
**********************************************************************/
TEST(forward_vector_dot_div, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        C[i] = A[i] / B[i];

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
    delete[] C;
}
TEST(forward_vector_dot_div, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    forward_vector_dot_div(C, A, B, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
    delete[] C;
}
TEST(forward_vector_dot_div, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float *C = new float[m * n];

    fill_random(A, m * n);
    fill_random(B, m * n);

    forward_vector_dot_div(C, A, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(C[i], A[i] / B[i]);
    }

    delete[] A;
    delete[] B;
    delete[] C;
}
/**********************************************************************
 * 
 *  vector dot-division backward test. (matrix A)
 * 
**********************************************************************/
TEST(backward_vector_dot_div_A, random_case0) {
    const int m = 16, n = 16;
    float *dC = new float[m * n];
    float *B = new float[m * n];
    float *d = new float[m * n];

    fill_random(dC, m * n);

    backward_vector_dot_div_A(d, B, dC, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(d[i], dC[i]/B[i]);
    }

    delete[] d;
    delete[] dC;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector dot-division backward test. (matrix B)
 * 
**********************************************************************/
TEST(backward_vector_dot_div_B, random_case0) {
    const int m = 16, n = 16;
    float *dC = new float[m * n];
    float *d = new float[m * n];
    float *B = new float[m * n];
    float *A = new float[m * n];

    fill_random(dC, m * n);

    backward_vector_dot_div_B(d, A, B, dC, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(d[i], -dC[i] * A[i]/B[i] / B[i] );
    }

    delete[] d;
    delete[] dC;
}

#endif // DO_VECTOR_DIV == 1


#if DO_VECTOR_SCALE == 1
/**********************************************************************
 * 
 *  vector scale forward test.
 * 
**********************************************************************/
TEST(forward_vector_scale, no_parallel_case0) {
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

TEST(forward_vector_scale, parallel_case1) {
    int m = 4096, n = 4096;
    float *matA = new float[m * n];
    float *matB = new float[m * n];

    forward_vector_scale(matB, matA, 2, m * n);

    EXPECT_EQ(1,1);

    delete[] matA;
    delete[] matB;
}

TEST(forward_vector_scale, s1_case2) {
    int m = 16, n = 16;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    fill_random(matA, m * n);

    forward_vector_scale(matB, matA, 1, m * n);

    for(int i = 0; i < m * n; i++) {
        EXPECT_EQ(matA[i], matB[i]);
    }

    delete[] matA;
    delete[] matB;
}

TEST(forward_vector_scale, s0_case3) {
    int m = 16, n = 16;
    float *matA = new float[m * n];
    float *matB = new float[m * n];
    fill_random(matA, m * n);

    forward_vector_scale(matB, matA, 0, m * n);

    for(int i = 0; i < m * n; i++) {
        EXPECT_EQ(0, matB[i]);
    }

    delete[] matA;
    delete[] matB;
}

/**********************************************************************
 * 
 *  vector scale backward test. (for matrix A)
 * 
**********************************************************************/
TEST(backward_vector_scale_A, specific_case0) {
    const int m = 4, n = 4;
    float *dA = new float[m * n];
    float dB[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float s = 3.1416;

    backward_vector_scale_A(dA, s, dB, m * n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dA[i],s,0.00005);
    }

    delete[] dA;
}
TEST(backward_vector_scale_A, random_case1) {
    const int m = 4, n = 4;
    float *dA = new float[m * n];
    float dB[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float s;

    fill_random(&s, 1);

    backward_vector_scale_A(dA, s, dB, m * n);

    for(int i = 0; i < m * n; ++i) {
        EXPECT_NEAR(dA[i],s,0.00005);
    }

    delete[] dA;
}

/**********************************************************************
 * 
 *  vector scale backward test. (for scalar s)
 * 
**********************************************************************/
TEST(backward_vector_scale_s, specific_case0) {
    const int m = 4, n = 4;
    float ds = 0;
    float dB[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float A[] = { 1.0648, 0.2398, -0.5973, 0.8604
                , -0.0897, 0.2770, -0.6439, 0.2394
                , 0.0973, 0.8857, 0.5930, 0.8655
                , -0.1681, 0.6876, 0.9303, -0.1169};

    backward_vector_scale_s(&ds, A, dB, m * n);

    EXPECT_NEAR(ds, 5.1248, 0.0002);
}
TEST(backward_vector_scale_s, random_case1) {
    const int m = 4, n = 4;
    float ds = 0;
    float dB[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
    float *A = new float[m * n];

    fill_random(A, m * n);

    backward_vector_scale_s(&ds, A, dB, m * n);

    float sum = 0;
    for(int i = 0; i < m*n; ++i)
        sum += A[i];

    EXPECT_NEAR(ds, sum, 0.00002);

    delete[] A;
}
#endif // DO_VECTOR_SCALE == 1


#if DO_VECTOR_ABS == 1
/**********************************************************************
 * 
 *  vector abs forward test.
 * 
**********************************************************************/
TEST(forward_vector_abs, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = fabsf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_abs, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_abs(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_abs, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_abs(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], fabsf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector abs backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_abs_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_abs_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * sign(A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_ABS == 1

#if DO_VECTOR_SIGN == 1
/**********************************************************************
 * 
 *  vector sign forward test.
 * 
**********************************************************************/
TEST(forward_vector_sign, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = sign(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_sign, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_sign(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_sign, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_sign(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], sign(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector sign backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_sign_A, random_case0) {
    const int m = 16, n = 16;
    float *dA = new float[m * n];

    backward_vector_sign_A(dA, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], 0);
    }

    delete[] dA;
}
#endif // DO_VECTOR_SIGN == 1

#if DO_VECTOR_EXP == 1
/**********************************************************************
 * 
 *  vector exp forward test.
 * 
**********************************************************************/
TEST(forward_vector_exp, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = expf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_exp, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_exp(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_exp, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_exp(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], expf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector exp backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_exp_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *B = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(B, m * n);

    backward_vector_exp_A(dA, dB, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * B[i]);
    }

    delete[] dA;
    delete[]  B;
    delete[] dB;
}
#endif // DO_VECTOR_EXP == 1

#if DO_VECTOR_EXPM1 == 1
/**********************************************************************
 * 
 *  vector forward test.
 * 
**********************************************************************/
TEST(forward_vector_expm1, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = expm1f(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_expm1, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_expm1(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_expm1, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_expm1(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], expm1f(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_expm1_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *B = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(B, m * n);

    backward_vector_expm1_A(dA, dB, B, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * (B[i] + 1));
    }

    delete[] dA;
    delete[]  B;
    delete[] dB;
}
#endif // DO_VECTOR_EXPM1 == 1

#if DO_VECTOR_LOG == 1
/**********************************************************************
 * 
 *  vector log forward test.
 * 
**********************************************************************/
TEST(forward_vector_log, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = logf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_log, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_log(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_log, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_log(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], logf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector log backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_log_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_log_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] / A[i]);
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_LOG == 1

#if DO_VECTOR_LOGIP == 1
/**********************************************************************
 * 
 *  vector log1p forward test.
 * 
**********************************************************************/
TEST(forward_vector_log1p, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = log1pf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_log1p, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_log1p(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_log1p, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_log1p(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], log1pf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector log1p backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_log1p_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_log1p_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] / (A[i] + 1));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_LOGIP == 1

#if DO_VECTOR_SQRT == 1
/**********************************************************************
 * 
 *  vector sqrt forward test.
 * 
**********************************************************************/
TEST(forward_vector_sqrt, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = sqrtf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_sqrt, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_sqrt(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_sqrt, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_sqrt(B, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], sqrtf(A[i]));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector sqrt backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_sqrt_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_sqrt_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] / 2 /  sqrtf(A[i]));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_SQRT == 1

#if DO_VECTOR_POW == 1
/**********************************************************************
 * 
 *  vector pow forward test.
 * 
**********************************************************************/
TEST(forward_vector_pow, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float x = 9.21924;

    for(int i = 0; i < m * n; ++i)
        B[i] = powf(A[i], x);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_pow, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float x = 9.21924;

    forward_vector_pow(B, A, x, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_pow, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];
    float x = 9.21924;

    fill_random(A, m * n);

    forward_vector_pow(B, A, x, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(B[i], powf(A[i],x));
    }

    delete[] A;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector pow backward test. (for vector A)
 * 
**********************************************************************/
TEST(backward_vector_pow_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];
    float  *B = new float[m * n];
    float x = 9.21924;

    fill_random(dB,m * n);
    fill_random(A,m * n);
    for(int i = 0; i < m * n; ++i)
        B[i] = powf(A[i],x);

    for(int i = 0; i < m * n; ++i)
        B[i] = powf(A[i], x);

    fill_random(dB, m * n);

    backward_vector_pow_A(dA, dB, A, B, x, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] * x * powf(A[i], x - 1));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
    delete[] B;
}
/**********************************************************************
 * 
 *  vector pow backward test. (for scalar x)
 * 
**********************************************************************/
TEST(backward_vector_pow_x, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float  dx;
    float  *B = new float[m * n];
    float x = 9.21924;

    fill_random(dB,m * n);
    fill_random(A,m * n);
    for(int i = 0; i < m * n; ++i)
        B[i] = powf(A[i], x);

    backward_vector_pow_w(&dx, dB, A, B, x, m * n);

    float sum = 0;
    for(int i = 0; i < m * n; ++i)
        sum += dB[i]* powf(A[i],x) * logf(A[i]);
    
    EXPECT_FLOAT_EQ(dx, sum);

    delete[]  B;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_POW == 1

#if DO_VECTOR_SIN == 1
/**********************************************************************
 * 
 *  vectorsin  forward test.
 * 
**********************************************************************/
TEST(forward_vector_sin, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = sinf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_sin, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_sin(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_sin, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_sin(B, A, m * n);

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
TEST(backward_vector_sin_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_sin_A(dA, dB, A, m * n);

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
TEST(forward_vector_cos, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = cosf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_cos, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_cos(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_cos, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_cos(B, A, m * n);

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
TEST(backward_vector_cos_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_cos_A(dA, dB, A, m * n);

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
TEST(forward_vector_tan, no_parallel_case0) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    for(int i = 0; i < m * n; ++i)
        B[i] = tanf(A[i]);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_tan, parallel_case1) {
    const int m = 8192, n = 8192;
    float *A = new float[m * n];
    float *B = new float[m * n];

    forward_vector_tan(B, A, m * n);

    EXPECT_EQ(1,1);

    delete[] A;
    delete[] B;
}
TEST(forward_vector_tan, random_case2) {
    const int m = 16, n = 16;
    float *A = new float[m * n];
    float *B = new float[m * n];

    fill_random(A, m * n);

    forward_vector_tan(B, A, m * n);

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
TEST(backward_vector_tan_A, random_case0) {
    const int m = 16, n = 16;
    float *dB = new float[m * n];
    float  *A = new float[m * n];
    float *dA = new float[m * n];

    fill_random(dB, m * n);
    fill_random(A, m * n);

    backward_vector_tan_A(dA, dB, A, m * n);

    for(int i = 0; i < m * n; ++i){
        EXPECT_FLOAT_EQ(dA[i], dB[i] / powf(cosf(A[i]),2));
    }

    delete[] dA;
    delete[]  A;
    delete[] dB;
}
#endif // DO_VECTOR_TAN == 1
