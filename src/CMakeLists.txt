# First try the automatic method
find_package(LLVM CONFIG QUIET)

if(NOT LLVM_FOUND)
  # Fall back to hardcoded paths if automatic detection fails
  message(WARNING "Automatic LLVM detection failed, trying known paths...")

  set(LLVM_PATHS
      "/opt/homebrew/opt/llvm/lib/cmake/llvm" "/usr/local/lib/cmake/llvm"
      "/usr/lib/llvm-19/lib/cmake/llvm" "/usr/lib/llvm/lib/cmake/llvm")

  find_package(LLVM CONFIG REQUIRED PATHS ${LLVM_PATHS} NO_DEFAULT_PATH)
endif()

message(STATUS "Found LLVM ${LLVM_VERSION} at ${LLVM_DIR}")

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
