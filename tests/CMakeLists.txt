# fetch googletest
include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1)
FetchContent_MakeAvailable(googletest)

# adding tests examples
add_executable(test_lexer test_lexer.cpp)

# link all libraries to the test executable
target_link_libraries(test_lexer PRIVATE optimizer_lib gtest_main)
enable_testing()

# Add the test
add_test(NAME LexerTest COMMAND test_lexer)
