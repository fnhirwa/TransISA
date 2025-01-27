# fetch googletest
include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1)
FetchContent_MakeAvailable(googletest)

# adding tests examples
add_executable(tests tests_main.cpp)

# link all libraries to the test executable
target_link_libraries(tests PRIVATE optimizer_lib gtest_main)
enable_testing()
# add the tests to the test suite
add_test(NAME TransISA_TESTS COMMAND tests)
