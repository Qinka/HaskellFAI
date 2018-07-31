#pragma once

#ifndef _TRIGONOMETRIC_FUNCTION_H_
#define _TRIGONOMETRIC_FUNCTION_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

//sin
void forward_N_sin(float *B, float *A, int n);
void backward_N_sin_A(float *dA, float *dB, float *A, int n);
//cos
void forward_N_cos(float *B, float *A, int n);
void backward_N_cos_A(float *dA, float *dB, float *A, int n);
//tan
void forward_N_tan(float *B, float *A, int n);
void backward_N_tan_A(float *dA, float *dB, float *B, int n);
//atan
void forward_N_atan(float *B, float *A, int n);
void backward_N_atan_A(float *dA, float *dB, float *A, int n);
//asin
void forward_N_asin(float *B, float *A, int n);
void backward_N_asin_A(float *dA, float *dB, float *A, int n);
//acos
void forward_N_acos(float *B, float *A, int n);
void backward_N_acos_A(float *dA, float *dB, float *A, int n);


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_TRIGONOMETRIC_FUNCTION_H_