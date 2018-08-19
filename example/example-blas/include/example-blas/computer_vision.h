#pragma once

#ifndef _COMPUTER_VISION_H_
#define _COMPUTER_VISION_H_

#ifdef __cplusplus
extern "C" {
#endif // ! __cplusplus

void color_background_mask(float *out, float *in, float *img_bg_color, int n, int en);
void color_background_mask_rgb_rang(float *out, float *in, int n,
                                    float aR, float aG, float aB,
                                    float bR, float bG, float bB);

#ifdef __cplusplus
}
#endif // ! __cplusplus

#endif // !_COMPUTER_VISION_H_