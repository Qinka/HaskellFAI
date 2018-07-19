#pragma once

#ifndef _ACTIVATION_FUNCTION_H_
#define _ACTIVATION_FUNCTION_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

void f_sigmoid(float *result, float *mat, int n);
void f_ReLU(float *result, float *mat, int n);
void f_softmax(float *result, float *mat, int n);


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_ACTIVATION_FUNCTION_H_