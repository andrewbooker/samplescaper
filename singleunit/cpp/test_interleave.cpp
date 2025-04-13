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

// passing
testOver(2)
testOver(4)
testOver(6)
testOver(10)
testOver(12)
testOver(14)
testOver(20)
testOver(30)


testOver(8)
testOver(16)
testOver(18)
testOver(22)
testOver(24)
testOver(26)
testOver(28)
testOver(40)
testOver(80)


