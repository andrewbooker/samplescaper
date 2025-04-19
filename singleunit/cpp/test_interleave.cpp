#include "interleave.h"

#include <iostream>
static bool can_interleave_buffer_of_length(int size) {
    float* buff(new float[size]);
    float* interleaved(new float[size]);

    for (int i(0); i != size; ++i) {
        buff[i] = 1.0 * i;
        if (i % 2 == 0) {
            interleaved[i] = i / 2.0;
        } else {
            interleaved[i] = ((size / 2.0) - 1) + ((i + 1) / 2.0);
        }
    }
    
    interleave(buff, size);
    bool passed(true);
    for (int i(0); i != size; ++i) {
        passed &= (interleaved[i] == buff[i]);
    }
    if (!passed) {
        std::cout << "expected=";
        for (int j(0); j != size; ++j) {
            std::cout << *(interleaved + j) << ",";
        }
        std::cout << "\n";
        std::cout << "     got=";
        for (int j(0); j != size; ++j) {
            std::cout << *(buff + j) << ",";
        }
        std::cout << "\n";
    }
    delete [] buff;
    delete [] interleaved;
    return passed;
}


#include <gtest/gtest.h>

#define testOver(n) TEST(InterleaveTest, over##n##Elements) { EXPECT_TRUE(can_interleave_buffer_of_length(n)); }

testOver(2)
testOver(4)
testOver(6)
testOver(8)
testOver(10)
testOver(12)
testOver(14)
testOver(16)
testOver(18)
testOver(22)
testOver(24)
testOver(26)
testOver(28)
testOver(40)

TEST(InterleaveTest, over12ElementsAcross3Channels) {
    float buff[12] {};
    float expected[12] {};
    const int channels(3);
    const int perBlock(12 / channels);
    for (int i(0); i != 12; ++i) {
        const int c(i % channels);
        const int p(i / channels);
        buff[i] = 1.0 * i;
        expected[i] = (1.0 * c * perBlock) + p;
    }
    float test[12] {0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11};
    for (int i(0); i != 12; ++i) {
        EXPECT_EQ(*(expected + i), *(test + i));
    }
}

TEST(InterleaveTest, over12ElementsAcross4Channels) {
    float buff[12] {};
    float expected[12] {};
    const int channels(4);
    const int perBlock(12 / channels);
    for (int i(0); i != 12; ++i) {
        const int c(i % channels);
        const int p(i / channels);
        buff[i] = 1.0 * i;
        expected[i] = (1.0 * c * perBlock) + p;
    }
    const float test[12] {0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 11};
    for (int i(0); i != 12; ++i) {
        EXPECT_EQ(*(expected + i), *(test + i));
    }
}


