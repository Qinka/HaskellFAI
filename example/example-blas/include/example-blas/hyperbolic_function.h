#pragma once

#ifndef _HYPERBOLIC_FUNCTION_H_
#define _HYPERBOLIC_FUNCTION_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

//cosh
void forward_N_cosh(float *B, float *A, int n);
void backward_N_cosh_A(float *dA, float *dB, float *A, int n);
//sinh
void forward_N_sinh(float *B, float *A, int n);
void backward_N_sinh_A(float *dA, float *dB, float *A, int n);
//tanh
void forward_N_tanh(float *B, float *A, int n);
void backward_N_tanh_A(float *dA, float *dB, float *B, int n);
//atanh
void forward_N_atanh(float *B, float *A, int n);
void backward_N_atanh_A(float *dA, float *dB, float *A, int n);
//acosh
void forward_N_acosh(float *B, float *A, int n);
void backward_N_acosh_A(float *dA, float *dB, float *A, int n);
//asinh
void forward_N_asinh(float *B, float *A, int n);
void backward_N_asinh_A(float *dA, float *dB, float *A, int n);


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_HYPERBOLIC_FUNCTION_H_