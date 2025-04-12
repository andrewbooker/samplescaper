#include "interleave.h"

#include <iostream>
static bool can_interleave_n_elements(int size) {
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
        std::cout << "got=";
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


TEST(InterleaveTest, test_posFrom) {
    EXPECT_EQ(posFrom(0, 1), 0);
    EXPECT_EQ(posFrom(0, 5), 0);
    EXPECT_EQ(posFrom(0, 10), 0);

    EXPECT_EQ(posFrom(1, 5), 2);
    EXPECT_EQ(posFrom(2, 5), 4);
    EXPECT_EQ(posFrom(3, 5), 6);
    EXPECT_EQ(posFrom(4, 5), 8);

    EXPECT_EQ(posFrom(5, 5), 1);
    EXPECT_EQ(posFrom(6, 5), 3);
    EXPECT_EQ(posFrom(7, 5), 5);
    EXPECT_EQ(posFrom(8, 5), 7);
    EXPECT_EQ(posFrom(9, 5), 9);
}

TEST(InterleaveTest, test_inversePosFrom) {
    EXPECT_EQ(inversePosFrom(0, 1), 0);
    EXPECT_EQ(inversePosFrom(0, 5), 0);
    EXPECT_EQ(inversePosFrom(0, 10), 0);

    EXPECT_EQ(inversePosFrom(1, 5), 5);
    EXPECT_EQ(inversePosFrom(2, 5), 1);
    EXPECT_EQ(inversePosFrom(3, 5), 6);
    EXPECT_EQ(inversePosFrom(4, 5), 2);
    EXPECT_EQ(inversePosFrom(5, 5), 7);
    EXPECT_EQ(inversePosFrom(6, 5), 3);
    EXPECT_EQ(inversePosFrom(7, 5), 8);
    EXPECT_EQ(inversePosFrom(8, 5), 4);
    EXPECT_EQ(inversePosFrom(9, 5), 9);
}


TEST(InterleaveTest, variousNumbersOfElements) {
    EXPECT_TRUE(can_interleave_n_elements(4));
    EXPECT_TRUE(can_interleave_n_elements(6));
    EXPECT_TRUE(can_interleave_n_elements(10));
    EXPECT_TRUE(can_interleave_n_elements(20));
}

TEST(InterleaveTest, eightElements) {
    EXPECT_TRUE(can_interleave_n_elements(8));
}

TEST(InterleaveTest, fortyElements) {
    EXPECT_TRUE(can_interleave_n_elements(40));
}

TEST(InterleaveTest, eightyElements) {
    EXPECT_TRUE(can_interleave_n_elements(80));
}
