# Check if LLVM is installed via Homebrew (common on macOS M1/M2)
if(EXISTS "/usr/local/lib/cmake/llvm")
  set(LLVM_DIR "/usr/local/lib/cmake/llvm")
  message(STATUS "Using LLVM from standard installation at /usr/local")
elseif(EXISTS "/opt/homebrew/opt/llvm/lib/cmake/llvm")
  set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm")
  message(
    STATUS "Using LLVM from Homebrew installation at /opt/homebrew/opt/llvm")
else()
  message(
    FATAL_ERROR
      "Could not find LLVM installation. Make sure LLVM is installed via Homebrew or in /usr/local."
  )
endif()

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Map LLVM components to library names
llvm_map_components_to_libnames(LLVM_LIBS support core irreader mc scalaropts)

# Add include directories
include_directories(${CMAKE_SOURCE_DIR}/include)

# Add subdirectories
add_subdirectory(lexer)
add_subdirectory(llvm_ir)
add_subdirectory(optimizer)
add_subdirectory(parser)

# Executable
add_executable(TransISA main.cpp)

# Link the libraries
target_link_libraries(TransISA PRIVATE optimizer_lib ${LLVM_LIBS} c++ c++abi)
