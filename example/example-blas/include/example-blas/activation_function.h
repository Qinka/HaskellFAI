#pragma once

#ifndef _ACTIVATION_FUNCTION_H_
#define _ACTIVATION_FUNCTION_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

// sigmoid
void forward_N_sigmoid(float *B, float *A, int n);
void backward_N_sigmoid_A(float *dA, float *dB, float *B, int n);
// ReLU
void forward_N_ReLU(float *B, float *A, int n);
void backward_N_ReLU_A(float *dA, float *dB, float *A, int n);


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_ACTIVATION_FUNCTION_H_