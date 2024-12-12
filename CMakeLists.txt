cmake_minimum_required(VERSION 3.16)
project(TransISA)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add source files
file(GLOB_RECURSE SOURCES "src/*.cpp")

# Include directories
include_directories(include)

# Add executable
add_executable(TransISA ${SOURCES})

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Link LLVM libraries
llvm_map_components_to_libnames(LLVM_LIBS support core irreader)
target_link_libraries(TransISA ${LLVM_LIBS})
