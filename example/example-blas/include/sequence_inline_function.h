#pragma once

#ifndef _SEQUENCE_INLINE_FUNCTION_H_
#define _SEQUENCE_INLINE_FUNCTION_H_

#include <config.h>

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus


inline float sign(float a) {
    return (.0f < a) - (a < .0f);
}


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_SEQUENCE_INLINE_FUNCTION_H_
