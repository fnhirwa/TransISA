cmake_minimum_required(VERSION 3.16)
project(TransISA VERSION 0.0.0)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# include directory
include_directories(include)

# Add subdirectories
add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(examples)
