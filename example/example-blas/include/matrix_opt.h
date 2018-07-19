#pragma once

#ifndef _MATRIX_OPT_H_
#define _MATRIX_OPT_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

/**
 * @brief function f_matrix_mul2D
 * $dst = A \times B$
 * 
 * @param dst The distance memory area
 * @param A input matrix 1
 * @param B input matrix 2
 * @param m A's rows
 * @param n A's cols (B's rows)
 * @param s B's cols
 */
void f_matrix_mul2D(float *dst, float *A, float *B, int m, int n, int s);

/**
 * @brief function f_matrix_dot_mul2D
 * $dst = A \cdot B$
 * 
 * @param dst The distance memory area
 * @param A input matrix 1
 * @param B input matrix 2
 * @param m rows
 * @param n cols
 */
void f_matrix_dot_mul2D(float *dst, float *A, float *B, int m, int n);

/**
 * @brief matrix scale
 * $dst = scale \cdot A$
 * 
 * @param dst distance matrix
 * @param A matrix 1
 * @param scale scaler
 * @param m row
 * @param n col
 */
void f_matrix_scale_mul2D(float *dst, float *A, float scale, int m, int n);

void f_matrix_add(float *sum, float *A, float *B, int n);
void f_matrix_sub(float *sub, float *A, float *B, int n);
void f_matrix_fill_eye(float *mat, int row_num);
void f_matrix_fill(float *mat, float f, int n);
void f_matrix_random(float *mat, int n);

#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_MATRIX_MULTIPLICATION_H_