#pragma once

#ifndef _SEQUENCE_INLINE_FUNCTION_H_
#define _SEQUENCE_INLINE_FUNCTION_H_

#include <config.h>

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

#define P1 1.0471975511965976f
#define P2 2.0943951023931953f
#define P3 3.141592653589793f
#define P4 4.1887902047863905f
#define P5 5.235987755982989f
#define P6 6.283185307179586f

#ifdef __DISABLEO3__
#define sign(a) ((.0f < a) - (a < .0f))
#define hpr(h) ((h <= P1) + (h >= P5)                  \
              + (h > P1 && h < P2) * (P2 - h) / P1     \
              + (h > P4 && h < P5) * (h - P4) / P1)
#define hpg(h) ((h >= P1 && h <= P3)                   \
              + (h < P1) * h / P1                      \
              + (h > P3 && h < P4) * (P4 - h) / P1)
#define hpb(h) ((h >=  P3 && h <=  P5)                 \
              + (h >  P2 && h <  P3) * (h - P2) / P1   \
              + (h >  P5 && h <  P6) * (P6 - h) / P1)
#else
inline float sign(float a) {
    return (.0f < a) - (a < .0f);
}
inline float hpr(float h) {
    return (h <= P1) + (h >= P5)
         + (h > P1 && h < P2) * (P2 - h) / P1
         + (h > P4 && h < P5) * (h - P4) / P1;
}
inline float hpg(float h) {
    return (h >= P1 && h <= P3)
         + (h < P1) * h / P1
         + (h > P3 && h < P4) * (P4 - h) / P1;
}
inline float hpb(float h) {
    return (h >= P3 && h <= P5)
         + (h >  P2 && h <  P3) * (h - P2) / P1
         + (h >  P5 && h <  P6) * (P6 - h) / P1;
}
#endif // __APPLE__


#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_SEQUENCE_INLINE_FUNCTION_H_
