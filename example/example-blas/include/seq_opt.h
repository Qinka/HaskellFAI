#pragma once

#ifndef _SEQ_OPT_H_
#define _SEQ_OPT_H_

#include <config.h>

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

// sign function
#ifdef _OACC_ENABLE_
#pragma acc routine seq
#endif
float sign(float a);

#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_SEQ_OPT_H_
