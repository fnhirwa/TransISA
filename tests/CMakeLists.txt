# fetch googletest
include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1)
FetchContent_MakeAvailable(googletest)

# Set LLVM directory Set LLVM directory
if(APPLE)
  # macOS - Homebrew installation (Apple Silicon or Intel)
  if(EXISTS "/opt/homebrew/opt/llvm@19/lib/cmake/llvm")
    set(LLVM_DIR "/opt/homebrew/opt/llvm@19/lib/cmake/llvm")
  elseif(EXISTS "/usr/local/opt/llvm@19/lib/cmake/llvm")
    set(LLVM_DIR "/usr/local/opt/llvm@19/lib/cmake/llvm")
  endif()
elseif(UNIX AND NOT APPLE)
  # Linux - Check common installation paths
  if(EXISTS "/usr/lib/llvm-19/lib/cmake/llvm")
    set(LLVM_DIR "/usr/lib/llvm-19/lib/cmake/llvm")
  elseif(EXISTS "/usr/local/lib/llvm-19/lib/cmake/llvm")
    set(LLVM_DIR "/usr/local/lib/llvm-19/lib/cmake/llvm")
  elseif(EXISTS "/usr/lib/llvm/lib/cmake/llvm")
    set(LLVM_DIR "/usr/lib/llvm/lib/cmake/llvm")
  elseif(EXISTS "/usr/local/lib/cmake/llvm")
    set(LLVM_DIR "/usr/local/lib/cmake/llvm")
  endif()
endif()

if(NOT DEFINED LLVM_DIR)
  message(
    FATAL_ERROR
      "LLVM directory not found. Set LLVM_DIR manually or install LLVM properly."
  )
endif()

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Map LLVM components to library names
llvm_map_components_to_libnames(
  LLVM_LIBS
  core
  support
  irreader
  mc
  target
  aarch64codegen
  aarch64desc
  aarch64asmparser
  aarch64disassembler
  aarch64info
  aarch64utils
  asmprinter
  bitwriter)

# Add test executables
add_executable(test_lexer test_lexer.cpp)
add_executable(test_parser test_parser.cpp)
add_executable(test_llvm_codegen test_llvm_codegen.cpp)

# Link all libraries to the test executables
target_link_libraries(test_lexer PRIVATE codegen_lib gtest_main)
target_link_libraries(test_parser PRIVATE codegen_lib gtest_main)
target_link_libraries(test_llvm_codegen PRIVATE codegen_lib gtest_main)

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
