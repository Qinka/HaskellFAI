#include "test_prelude.h"

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