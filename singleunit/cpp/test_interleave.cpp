#include "interleave.h"

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

TEST(InterleaveTest, fourElements) {
    float buff[] { 1.0, 2.0, 3.0, 4.0 };
    float interleaved[] { 1.0, 3.0, 2.0, 4.0 };

    interleave(buff, 4);
    for (int i(0); i != 4; ++i) {
        EXPECT_EQ(buff[i], interleaved[i]);
    }
}


TEST(InterleaveTest, tenElements) {
    const unsigned int size(10);
    float buff[] { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 };
    float interleaved[] { 1.0, 6.0, 2.0, 7.0, 3.0, 8.0, 4.0, 9.0, 5.0, 10.0 };

    interleave(buff, size);
    for (int i(0); i != size; ++i) {
        EXPECT_EQ(buff[i], interleaved[i]);
    }
}

TEST(InterleaveTest, twentyElements) {
    const unsigned int size(20);
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
    bool failed(false);
    for (int i(0); i != size; ++i) {
        failed |= (interleaved[i] != buff[i]);
    }
    EXPECT_EQ(false, failed);
    if (failed) {
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
}
