#include "interleave.h"

#include <gtest/gtest.h>

TEST(InterleaveTest, fourElements) {
    float buff[] { 1.0, 2.0, 3.0, 4.0 };
    float interleaved[] { 1.0, 3.0, 2.0, 4.0 };

    interleave(buff, 4);
    EXPECT_EQ(buff, interleaved);
}
