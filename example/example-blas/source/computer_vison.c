#include <example-blas/activation_function.h>
#include <config.h>
#include <math.h>

void color_background_mask(float *out, float *in, float *img_bg_color, int n, int en) {
    
    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(out, in, img_bg_color, n, en)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(out, in, img_bg_color, n, en)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(out[0:en*n]), copyin(in[0:en*n], img_bg_color[0:en])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(out, in, img_bg_color)
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++) {
            float mask = 1;
            int j;
            #if   ACC_LOOP == OACC_ENABLE
            #pragma acc loop
            #endif
            for(j = 0; j < en; j ++) {
                mask *= in[j + i * en] == img_bg_color[j];
            }
            mask = 1 - mask;
            #if   ACC_LOOP == OACC_ENABLE
            #pragma acc loop
            #endif
            for(j = 0; j < en; j ++) {
                out[j + i * en] = mask;
            }
        }
    }
}


void color_background_mask_rgb_rang(float *out, float *in, int n,
                                    float aR, float aG, float aB,
                                    float bR, float bG, float bB) {    

    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(out, in, n, aR, aG, aB, bR, bG, bB)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(out, in, n, aR, aG, aB, bR, bG, bB)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(out[0:3*n]), copyin(in[0:3*n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(out, in)
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for (i = 0; i < n; i++) {
            float  mask = (float)(aR <= in[i * 3 + 0] && bR >= in[i * 3 + 0]
                               && aG <= in[i * 3 + 1] && bG >= in[i * 3 + 1]
                               && aB <= in[i * 3 + 2] && bB >= in[i * 3 + 2]);
            mask = 1 - mask;
            out[i * 3 + 0] = mask;
            out[i * 3 + 1] = mask;
            out[i * 3 + 2] = mask;
        }
    }
}

void color_rgb_to_hsv(float *out, float *in, int n){
    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(out, in, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(out, in, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(out[0:3*n]), copyin(in[0:3*n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(out, in)
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for(i = 0; i < n; ++i) {
            float r = in[i * 3 + 0],
                  g = in[i * 3 + 1],
                  b = in[i * 3 + 2];
            float p = acosf((r -g) * (r - b) / 2 / sqrtf((r -g) * (r - g) + (r - b) * (g - b)));
            out[i * 3 + 0] = (b < g) * p + (b >= g) * (6.283185307179586f - p);
            float max = (r >= b) * r + (b > r) * b;
            max = (g >= max) * g + (max > g) * max;
            float min = (r <= b) * r + (b < r) * b;
            min = (g <= min) * g + (min < g) * min;
            out[i * 3 + 1] = (max - min) / max;
            out[i * 3 + 2] = max;
        }
    }
}

void color_hsv_to_rgb(float *out, float *in, int n){
    #if   ACC_REGION == OMP_ONLY
    #pragma omp parallel shared(out, in, n)
    #elif ACC_REGION == OMP_TARGET
    #pragma omp target parallel shared(out, in, n)
    #elif ACC_REGION == OACC_ONLY
    #pragma acc data copyout(out[0:3*n]), copyin(in[0:3*n])
    #elif ACC_REGION == OACC_DRVPTR
    #pragma acc data deviceptr(out, in)
    #endif
    {
        int i;

        #if   ACC_LOOP == OMP_ENABLE
        #pragma omp for
        #elif ACC_LOOP == OACC_ENABLE
        #pragma acc parallel loop
        #endif
        for(i = 0; i < n; ++i) {
            float r = in[i * 3 + 0],
                  g = in[i * 3 + 1],
                  b = in[i * 3 + 2];
            float p = acosf((r -g) * (r - b) / 2 / sqrtf((r -g) * (r - g) + (r - b) * (g - b)));
            out[i * 3 + 0] = (b < g) * p + (b >= g) * (6.283185307179586f - p);
            float max = (r >= b) * r + (b > r) * b;
            max = (g >= max) * g + (max > g) * max;
            float min = (r <= b) * r + (b < r) * b;
            min = (g <= min) * g + (min < g) * min;
            out[i * 3 + 1] = (max - min) / max;
            out[i * 3 + 2] = max;
        }
    }
}