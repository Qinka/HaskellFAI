#pragma once

#ifndef _VECTOR_OPT_H_
#define _VECTOR_OPT_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

// Add
/**
 * @brief $C = A + B$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_vector_add(float *C, float *A, float *B, int n);
/**
 * @brief $\frac{\partial L}{\partial A} = \frac{\partial L}{\partial C}$
 * 
 * @param dA gradient matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_vector_add_A(float *dA, float *dC, int n);
/**
 * @brief $\frac{\partial L}{\partial B} = \frac{\partial L}{\partial C}$
 * 
 * @param dB gradient matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_vector_add_B(float *dB, float *dC, int n);

// Sub
/**
 * @brief $C = A - B$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_vector_sub(float *C, float *A, float *B, int n);
/**
 * @brief $\frac{\partial L}{\partial A} = \frac{\partial L}{\partial C}$
 * 
 * @param dA gradient matrix
 * @param dC gradient matrix
 * @param n 
 */
void backward_vector_sub_A(float *dA, float *dC, int n);
/**
 * @brief $\frac{\partial L}{\partial B} = - \frac{\partial L}{\partial C}$
 * 
 * @param dB gradient matrix
 * @param dC gradient matrix
 * @param n 
 */
void backward_vector_sub_B(float *dB, float *dC, int n);

// dot product
/**
 * @brief $C = A \cdot B$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_vector_dot_prd(float *C, float *A, float *B, int n);
/**
 * @brief $\frac{\partial L}{\partial A} = B \cdot \frac{\partial L}{\partial C}$
 * 
 * @param dA gradient matrix
 * @param B matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_vector_dot_prd_A(float *dA, float *B, float *dC, int n);
/**
 * @brief $\frac{\partial L}{\partial B} = A \cdot \frac{\partial L}{\partial C}$
 * 
 * @param dB gradient matrix
 * @param A matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_vector_dot_prd_B(float *dB, float *A, float *dC, int n);

// dot division
/**
 * @brief $\{c_i = a_i / b_i\}$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_vector_dot_div(float *C, float *A, float *B, int n);
/**
 * @brief $\{\frac{\partial l}{\partial a_i} = \frac{1}{b_i} \frac{\partial l}{\partial c_i}\}$
 * 
 * @param dA gradient matrix
 * @param B matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_vector_dot_div_A(float *dA, float *B, float *dC, int n);
/**
 * @brief $\{\frac{\partial l}{\partial a_i} = -\frac{a_i}{b^2_i} \frac{\partial l}{\partial c_i}\}$
 * 
 * @param dB gradient matrix
 * @param A matrix
 * @param B matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_vector_dot_div_B(float *dB, float *A, float *B, float *dC, int n);

// scale
/**
 * @brief $B = sA$
 * 
 * @param B matrix
 * @param A matrix
 * @param s scalar
 * @param n size
 */
void forward_vector_scale(float *B, float *A, float s, int n);
/**
 * @brief $\frac{\partial L}{\partial S} = s\frac{\partial L}{\partial B}$
 * 
 * @param dA gradient matrix
 * @param s scalar
 * @param dB gradient matrix
 * @param n size
 */
void backward_vector_scale_A(float *dA, float s, float *dB, int n);
/**
 * @brief $\frac{\partial L}{\partial s} = \frac{\partial L}{\partial B}\sum a_i$
 * 
 * @param ds gradient 
 * @param A matrix scalar
 * @param dB gradient matrix
 * @param n size
 */
void backward_vector_scale_s(float *ds, float *A, float *dB, int n);

//abs
void forward_vector_abs(float *B, float *A, int n);
void backward_vector_abs_A(float *dA, float *dB, float *A, int n);
//sign
void forward_vector_sign(float *B, float *A, int n);
void backward_vector_sign_A(float *dA, int n);
//exp
void forward_vector_exp(float *B, float *A, int n);
void backward_vector_exp_A(float *dA, float *dB, float *B, int n);
//expm1
void forward_vector_expm1(float *B, float *A, int n);
void backward_vector_expm1_A(float *dA, float *dB, float *B, int n);
//log
void forward_vector_log(float *B, float *A, int n);
void backward_vector_log_A(float *dA, float *dB, float *A, int n);
//logp1
void forward_vector_log1p(float *B, float *A, int n);
void backward_vector_log1p_A(float *dA, float *dB, float *A, int n);
//sqrt
void forward_vector_sqrt(float *B, float *A, int n);
void backward_vector_sqrt_A(float *dA, float *dB, float *A, int n);
//pow
void forward_vector_pow(float *B, float *A, float x, int n);
void backward_vector_pow_A(float *dA, float *dB, float *A, float *B, float x, int n);
void backward_vector_pow_w(float *dx, float *dB, float *A, float *B, float x, int n);
//sin
void forward_vector_sin(float *B, float *A, int n);
void backward_vector_sin_A(float *dA, float *dB, float *A, int n);
//cos
void forward_vector_cos(float *B, float *A, int n);
void backward_vector_cos_A(float *dA, float *dB, float *A, int n);
//tan
void forward_vector_tan(float *B, float *A, int n);
void backward_vector_tan_A(float *dA, float *dB, float *B, int n);

// sign function
float sign(float a);

#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_VECTOR_OPT_H_
