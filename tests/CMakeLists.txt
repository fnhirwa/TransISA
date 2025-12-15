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
  # Core and IR
  support
  core
  irreader
  mc
  scalaropts
  asmparser
  asmprinter
  # aarch64 backend
  aarch64asmparser
  aarch64codegen
  aarch64desc
  aarch64disassembler
  aarch64info
  aarch64utils
  bitwriter
  # ARM backend ARM ARMCodeGen ARMAsmParser ARMDesc ARMDisassembler ARMInfo
  # ARMUtils # X86 backend
  x86asmparser
  x86codegen
  x86desc
  x86disassembler
  x86info)

# Add test executables
add_executable(test_frontend test_frontend.cpp)

# Link all libraries to the test executables
target_link_libraries(
  test_frontend PRIVATE frontend_lib codegen_lib optimizer_lib utils_lib
                        gtest_main ${LLVM_LIBS})

# Enable testing and discover tests
enable_testing()
include(GoogleTest)
gtest_discover_tests(test_frontend)
