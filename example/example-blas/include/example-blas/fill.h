#pragma once

#ifndef _FILL_H_
#define _FILL_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

void fill_N_scalar(float *A, float s, int n);
void fill_HW_eye(float *A, int h);
void fill_N_random(float *A, int n);

#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_FILL_H_