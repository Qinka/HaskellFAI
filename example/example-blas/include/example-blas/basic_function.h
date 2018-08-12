#pragma once

#ifndef _BASIC_FUNCTION_H_
#define _BASIC_FUNCTION_H_

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
void forward_N_add(float *C, float *A, float *B, int n);
/**
 * @brief $\frac{\partial L}{\partial A} = \frac{\partial L}{\partial C}$
 * 
 * @param dA gradient matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_N_add_A(float *dA, float *dC, int n);
/**
 * @brief $\frac{\partial L}{\partial B} = \frac{\partial L}{\partial C}$
 * 
 * @param dB gradient matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_N_add_B(float *dB, float *dC, int n);

// Sub
/**
 * @brief $C = A - B$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_N_sub(float *C, float *A, float *B, int n);
/**
 * @brief $\frac{\partial L}{\partial A} = \frac{\partial L}{\partial C}$
 * 
 * @param dA gradient matrix
 * @param dC gradient matrix
 * @param n 
 */
void backward_N_sub_A(float *dA, float *dC, int n);
/**
 * @brief $\frac{\partial L}{\partial B} = - \frac{\partial L}{\partial C}$
 * 
 * @param dB gradient matrix
 * @param dC gradient matrix
 * @param n 
 */
void backward_N_sub_B(float *dB, float *dC, int n);

// dot product
/**
 * @brief $C = A \cdot B$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_N_dot_prd(float *C, float *A, float *B, int n);
/**
 * @brief $\frac{\partial L}{\partial A} = B \cdot \frac{\partial L}{\partial C}$
 * 
 * @param dA gradient matrix
 * @param B matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_N_dot_prd_A(float *dA, float *B, float *dC, int n);
/**
 * @brief $\frac{\partial L}{\partial B} = A \cdot \frac{\partial L}{\partial C}$
 * 
 * @param dB gradient matrix
 * @param A matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_N_dot_prd_B(float *dB, float *A, float *dC, int n);

// dot division
/**
 * @brief $\{c_i = a_i / b_i\}$
 * 
 * @param C matrix
 * @param A matrix
 * @param B matrix
 * @param n size
 */
void forward_N_dot_div(float *C, float *A, float *B, int n);
/**
 * @brief $\{\frac{\partial l}{\partial a_i} = \frac{1}{b_i} \frac{\partial l}{\partial c_i}\}$
 * 
 * @param dA gradient matrix
 * @param B matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_N_dot_div_A(float *dA, float *B, float *dC, int n);
/**
 * @brief $\{\frac{\partial l}{\partial a_i} = -\frac{a_i}{b^2_i} \frac{\partial l}{\partial c_i}\}$
 * 
 * @param dB gradient matrix
 * @param A matrix
 * @param B matrix
 * @param dC gradient matrix
 * @param n size
 */
void backward_N_dot_div_B(float *dB, float *A, float *B, float *dC, int n);

// scale
/**
 * @brief $B = sA$
 * 
 * @param B matrix
 * @param A matrix
 * @param s scalar
 * @param n size
 */
void forward_N_scale(float *B, float *A, float *s, int n);
/**
 * @brief $\frac{\partial L}{\partial S} = s\frac{\partial L}{\partial B}$
 * 
 * @param dA gradient matrix
 * @param s scalar
 * @param dB gradient matrix
 * @param n size
 */
void backward_N_scale_A(float *dA, float *s, float *dB, int n);
/**
 * @brief $\frac{\partial L}{\partial s} = \frac{\partial L}{\partial B}\sum a_i$
 * 
 * @param ds gradient 
 * @param A matrix scalar
 * @param dB gradient matrix
 * @param n size
 */
void backward_N_scale_s(float *ds, float *A, float *dB, int n);

//abs
void forward_N_abs(float *B, float *A, int n);
void backward_N_abs_A(float *dA, float *dB, float *A, int n);
//sign
void forward_N_sign(float *B, float *A, int n);
void backward_N_sign_A(float *dA, float *dB, int n);
//exp
void forward_N_exp(float *B, float *A, int n);
void backward_N_exp_A(float *dA, float *dB, float *B, int n);
//expm1
void forward_N_expm1(float *B, float *A, int n);
void backward_N_expm1_A(float *dA, float *dB, float *B, int n);
//log
void forward_N_log(float *B, float *A, int n);
void backward_N_log_A(float *dA, float *dB, float *A, int n);
//logp1
void forward_N_log1p(float *B, float *A, int n);
void backward_N_log1p_A(float *dA, float *dB, float *A, int n);
//sqrt
void forward_N_sqrt(float *B, float *A, int n);
void backward_N_sqrt_A(float *dA, float *dB, float *A, int n);
//pow
void forward_N_pow(float *B, float *A, float *x, int n);
void backward_N_pow_A(float *dA, float *dB, float *A, float *B, float *x, int n);
void backward_N_pow_w(float *dx, float *dB, float *A, float *B, float *x, int n);
//ceil
void forward_N_ceil(float *B, float *A, int n);
void backward_N_ceil_A(float *dA, float *dB, int n);
//floor
void forward_N_floor(float *B, float *A, int n);
void backward_N_floor_A(float *dA, float *dB, int n);
//erf
void forward_N_erf(float *B, float *A, int n);
void backward_N_erf_A(float *dA, float *dB, float *A, int n);
//erfc
void forward_N_erfc(float *B, float *A, int n);
void backward_N_erfc_A(float *dA, float *dB, float *A, int n);

//max
void forward_N_max(float *C, float *A, float *B, int n);
void backward_N_max_A(float *dA, float *dC, float *A, float *B, int n);
void backward_N_max_B(float *dB, float *dC, float *A, float *B, int n);
//min
void forward_N_min(float *C, float *A, float *B, int n);
void backward_N_min_A(float *dA, float *dC, float *A, float *B, int n);
void backward_N_min_B(float *dB, float *dC, float *A, float *B, int n);

#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_BASIC_FUNCTION_H_
