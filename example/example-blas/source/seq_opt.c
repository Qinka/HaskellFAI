#include <seq_opt.h>

#if ACC_LOOP == OACC_ENABLE
#pragma acc routine seq
#endif
float sign(float a) {
    return (.0f < a) - (a < .0f);
}
