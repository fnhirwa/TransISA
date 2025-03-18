# fetch googletest
include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1)
FetchContent_MakeAvailable(googletest)

# Set LLVM directory
set(LLVM_DIR "/usr/local/lib/cmake/llvm")

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Map LLVM components to library names
llvm_map_components_to_libnames(LLVM_LIBS support core irreader mc scalaropts)

# Add test executables
add_executable(test_lexer test_lexer.cpp)
add_executable(test_parser test_parser.cpp)
add_executable(test_llvm_codegen test_llvm_codegen.cpp)

# Link all libraries to the test executables
target_link_libraries(test_lexer PRIVATE optimizer_lib gtest_main)
target_link_libraries(test_parser PRIVATE optimizer_lib gtest_main)
target_link_libraries(test_llvm_codegen PRIVATE optimizer_lib gtest_main
                                                ${LLVM_LIBS})

# Enable testing
enable_testing()

# Add individual tests
add_test(NAME LexerTest COMMAND test_lexer)
add_test(NAME ParserTest COMMAND test_parser)
add_test(NAME IRGenTest COMMAND test_llvm_codegen)

# Add a custom target to run all tests
add_custom_target(
  run_all_tests
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
  DEPENDS test_lexer test_parser test_llvm_codegen
  COMMENT "Running all tests...")
