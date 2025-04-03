cmake_minimum_required(VERSION 3.15)

project(TransISA)

# Set the LLVM directory
set(LLVM_DIR "/opt/homebrew/opt/llvm@19/lib/cmake/llvm")

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Set Clang as the compiler
set(CMAKE_C_COMPILER "/opt/homebrew/opt/llvm@19/bin/clang")
set(CMAKE_CXX_COMPILER "/opt/homebrew/opt/llvm@19/bin/clang++")

# Ensure that libc++ is used
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
include_directories(/opt/homebrew/opt/llvm@19/include/c++/v1)

# Map LLVM components to library names
llvm_map_components_to_libnames(LLVM_LIBS support core irreader mc scalaropts)

# Add include directories
include_directories(${CMAKE_SOURCE_DIR}/include)

# Subdirectories
add_subdirectory(lexer)
add_subdirectory(llvm_ir)
add_subdirectory(optimizer)
add_subdirectory(parser)

# Executable
add_executable(TransISA main.cpp)

# Link the libraries
target_link_libraries(TransISA PRIVATE optimizer_lib ${LLVM_LIBS})
