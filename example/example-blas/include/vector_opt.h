#pragma once

#ifndef _VECTOR_OPT_H_
#define _VECTOR_OPT_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

// Add
void forward_vector_add(float *C, float *A, float *B, int n);
void backward_vector_add_A(float *dA, float *dC, int n);
void backward_vector_add_B(float *dB, float *dC, int n);

// Sub
void forward_vector_sub(float *C, float *A, float *B, int n);
void backward_vector_sub_A(float *dA, float *dC, int n);
void backware_vector_sub_B(float *dB, float *dC, int n);

// dot product
void forward_vector_dot_prd(float *C, float *A, float *B, int n);
void backward_vector_dot_prd_A(float *dA, float *B, float *dC, int n);
void backward_vector_dot_prd_B(float *dB, float *A, float *dC, int n);

// dot product
void forward_vector_dot_div(float *C, float *A, float *B, int n);
void backward_vector_dot_div_A(float *dA, float *B, float *dC, int n);
void backward_vector_dot_div_B(float *dB, float *A, float *B, float *dC, int n);

// scale


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_VECTOR_OPT_H_