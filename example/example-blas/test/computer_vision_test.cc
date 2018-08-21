#include <test_prelude.h>
#include <example-blas/computer_vision.h>
#include <sequence_inline_function.h>

const int test_len = 16;
float testrgb[] = { 0, 0, 0 // black
                  , 1, 1, 1 // white
                  , 1, 0, 0 // red
                  , 0, 1, 0 // lime
                  , 0, 0, 1 // blue
                  , 1, 1, 0 // yellow
                  , 0, 1, 1 // cyan
                  , 1, 0, 1 // magenta
                  , 0.75, 0.75, 0.75 // silver
                  , 0.5, 0.5, 0.5 // gray
                  , 0.5,   0,   0 // maroon
                  , 0.5, 0.5,   0 // olive
                  ,   0, 0.5,   0 // green
                  , 0.5,   0, 0.5 // purple
                  ,   0, 0.5, 0.5 // teal
                  ,   0,   0, 0.5 // navy
                  };

float testhsv[] = { 0, 0, 0 // black
                  , 0, 0, 1 // white
                  , 0, 1, 1 // red
                  , P2, 1, 1 // lime
                  , P4, 1, 1 // blue
                  , P1, 1, 1 // yellow
                  , P3, 1, 1 // cyan
                  , P5, 1, 1 // magenta
                  , 0, 0, 0.75 // silver
                  , 0, 0, 0.5 // gray
                  , 0, 1, 0.5 // maroon
                  , P1, 1, 0.5 // olive
                  , P2, 1, 0.5 // green
                  , P5, 1, 0.5 // purple
                  , P3, 1, 0.5 // teal
                  , P4, 1, 0.5 //navy
                  };


#if DO_COLOR_RGB2HSV == 1
TEST(color_rgb_to_hsv, specific_case_0) {
  float* rt = new float[test_len * 3];
  color_rgb_to_hsv(rt, testrgb, test_len);

  for(int i = 0; i < test_len ; ++i) {
    EXPECT_FLOAT_NEAR(testhsv[i * 3 + 0], rt[i * 3 + 0]);
    EXPECT_FLOAT_NEAR(testhsv[i * 3 + 1], rt[i * 3 + 1]);
    EXPECT_FLOAT_NEAR(testhsv[i * 3 + 2], rt[i * 3 + 2]);
  }
  delete[] rt;
}
#endif

#if DO_COLOR_HSV2RGB == 1
TEST(color_hsb_to_rgb, specific_case_0) {
  float* rt = new float[test_len * 3];
  color_hsv_to_rgb(rt, testhsv, test_len);

  for(int i = 0; i < test_len ; ++i) {
    std::cout << testhsv[i * 3 + 0] << " " << testhsv[i * 3 + 1] << " " << testhsv[i * 3 + 2] << std::endl;
    std::cout << rt[i * 3 + 0] << " " << rt[i * 3 + 1] << " " << rt[i * 3 + 2] << std::endl;
    std::cout << "\t\t" << hpg(0) << std::endl;
    EXPECT_FLOAT_NEAR(testrgb[i * 3 + 0], rt[i * 3 + 0]);
    EXPECT_FLOAT_NEAR(testrgb[i * 3 + 1], rt[i * 3 + 1]);
    EXPECT_FLOAT_NEAR(testrgb[i * 3 + 2], rt[i * 3 + 2]);
  }
  delete[] rt;
}
#endif