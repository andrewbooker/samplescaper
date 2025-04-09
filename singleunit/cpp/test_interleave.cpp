#include "interleave.h"

#include <gtest/gtest.h>

TEST(InterleaveTest, fourElements) {
    float buff[] { 1.0, 2.0, 3.0, 4.0 };
    float interleaved[] { 1.0, 3.0, 2.0, 4.0 };

    interleave(buff, 4);
    for (int i(0); i != 4; ++i) {
        EXPECT_EQ(buff[i], interleaved[i]);
    }
}

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
