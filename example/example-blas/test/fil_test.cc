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