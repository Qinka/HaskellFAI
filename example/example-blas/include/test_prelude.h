#pragma once

#ifndef _TEST_PRELUDE_H_
#define _TEST_PRELUDE_H_

#include <gtest/gtest.h>
//#include <stdint.h>
//#include <cstdlib>
//#include <ctime>
#include <iostream>
//#include <cstdarg>
//#include <functional>
#include <random>
#include <cmath>

#define EXPECT_FLOAT_NEAR(x,y) EXPECT_NEAR((x),(y), fabsf(((x) + (y)) * 0.000006))


void fill_eye(float * mat, int row_num);
void fill_zero(float *mat, int num);
void fill_random(float *mat, int num);
void copy(float *dstMat, float *srcMat, int num);
void fill_with(float *mat, float f, int num);

// Enable the tests
#define DO_MATRIX_PRODUCT   1
#define DO_VECTOR_ADD       1
#define DO_VECTOR_SUB       1
#define DO_VECTOR_PRD       1
#define DO_VECTOR_DIV       1
#define DO_VECTOR_SCALE     1
#define DO_VECTOR_ABS       1
#define DO_VECTOR_SIGN      1
#define DO_VECTOR_EXP       1
#define DO_VECTOR_EXPM1     1
#define DO_VECTOR_LOG       1
#define DO_VECTOR_LOG1P     1
#define DO_VECTOR_SQRT      1
#define DO_VECTOR_POW       1
#define DO_VECTOR_SIN       1
#define DO_VECTOR_COS       1
#define DO_VECTOR_TAN       1
#define DO_VECTOR_ASIN      1
#define DO_VECTOR_ACOS      1
#define DO_VECTOR_ATAN      1
#define DO_VECTOR_SINH      1
#define DO_VECTOR_COSH      1
#define DO_VECTOR_TANH      1
#define DO_VECTOR_ASINH     1
#define DO_VECTOR_ACOSH     1
#define DO_VECTOR_ATANH     1
#define DO_VECTOR_CEIL      1
#define DO_VECTOR_FLOOR     1
#define DO_VECTOR_ERF       1
#define DO_VECTOR_ERFC      1
#define DO_VECTOR_SIGMOID   1
#define DO_VECTOR_RELU      1
#define DO_FILL_EYE         1
#define DO_VECTOR_MAX       1
#define DO_VECTOR_MIN       1
#define DO_COLOR_RGB2HSV    1
#define DO_COLOR_HSV2RGB    1

#endif // ! _TEST_PRELUDE_H_
