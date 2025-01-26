#include <gtest/gtest.h>


// a simple test with std output
void test1() {
    EXPECT_EQ(1, 1);
}
int main(int argc, char** argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}