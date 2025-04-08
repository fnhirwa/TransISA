# Set LLVM directory
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

if(DEFINED ENV{CMAKE_OSX_ARCHITECTURES})
  set(CMAKE_OSX_ARCHITECTURES $ENV{CMAKE_OSX_ARCHITECTURES})
else()
  set(CMAKE_OSX_ARCHITECTURES "arm64")
endif()

# Detect Platform
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  message(STATUS "Building for macOS")

  # Set architecture (arm64 or x86_64) based on environment variable
  if(DEFINED ENV{CMAKE_OSX_ARCHITECTURES})
    set(CMAKE_OSX_ARCHITECTURES $ENV{CMAKE_OSX_ARCHITECTURES})
    message(STATUS "Target architecture: ${CMAKE_OSX_ARCHITECTURES}")
  endif()

  # Optimization flags for macOS arm64
  if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
    add_compile_options(-mcpu=apple-m1 -O3)
  endif()

elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  message(STATUS "Building for Linux")

  # Optimization flags for Linux x86_64
  add_compile_options(-march=native -O3)
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

# Add include directories
include_directories(${CMAKE_SOURCE_DIR}/include)

# subdirectories
add_subdirectory(lexer)
add_subdirectory(llvm_ir)
add_subdirectory(codegen)
add_subdirectory(parser)

# Executable
add_executable(TransISA main.cpp)

# Link the libraries
target_link_libraries(TransISA PRIVATE codegen_lib ${LLVM_LIBS})
