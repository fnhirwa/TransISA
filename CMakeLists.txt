cmake_minimum_required(VERSION 3.16)
project(TransISA VERSION 0.0.0)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

if(APPLE)
  # Use a CMake variable instead of shell command
  include_directories(SYSTEM "${LLVM_INCLUDE_DIR}/c++/v1")
  link_directories("${LLVM_LIB_DIR}")
  add_compile_options(-stdlib=libc++)
  add_link_options(-stdlib=libc++)
endif()

# include directory
include_directories(include)

# Enable testing (must be in root CMakeLists.txt for CTest to discover tests)
enable_testing()

# Add subdirectories
add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(examples)
