#include <test_prelude.h>
#include <example-blas/fill.h>
#include <sequence_inline_function.h>

#if DO_FILL_EYE == 1
TEST(fill_HW_eye, case_0) {
    const int h = 10;
    float *A = new float[h * h];
    
    fill_HW_eye(A, h);

    for(int i = 0; i < h; i++)
        for(int j = 0; j < h; j++) {
            EXPECT_FLOAT_NEAR(A[i * h + j], (i == j ? 1 : 0));
        }

    delete[] A;
}
#endif // DO_FILL_EYE == 1

#if DO_FILL_SCALAR == 1
TEST(fill_N_scalar, case_0) {
    const int m = 10;
    float *A = new float[m * m];
    
    fill_N_scalar(A, 9.12924, m * m);

    delete[] A;
}
#endif // DO_FILL_EYE == 1


#if DO_FILL_RANDOM == 1
TEST(fill_N_random, case_0) {
    const int h = 10;
    float *A = new float[h * h];
    
    ffill_N_random(A, h * h);

    delete[] A;
}
#endif // DO_FILL_EYE == 1
