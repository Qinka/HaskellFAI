#pragma once

#ifndef _TEST_PRELUDE_H_
#define _TEST_PRELUDE_H_

#include <gtest/gtest.h>
#include <stdint.h>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <cstdarg>
#include <functional>
#include <random>

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

#endif // ! _TEST_PRELUDE_H_