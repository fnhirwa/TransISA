# Automatically detect LLVM installation path
if(EXISTS "/usr/local/lib/cmake/llvm") # Standard installation path (Linux &
                                       # macOS)
  set(LLVM_DIR "/usr/local/lib/cmake/llvm")
  message(STATUS "Using LLVM from standard installation at /usr/local")
elseif(EXISTS "/opt/homebrew/opt/llvm/lib/cmake/llvm") # macOS (Homebrew on
                                                       # Apple Silicon)
  set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm")
  message(
    STATUS "Using LLVM from Homebrew installation at /opt/homebrew/opt/llvm")

elseif(EXISTS "/usr/lib/llvm-19/lib/cmake/llvm") # Typical Linux path (e.g.,
                                                 # Ubuntu/Debian with apt
                                                 # install)
  set(LLVM_DIR "/usr/lib/llvm-19/lib/cmake/llvm")
  message(STATUS "Using LLVM from system installation at /usr/lib/llvm-19")

elseif(EXISTS "/usr/lib/llvm/lib/cmake/llvm") # Generic Linux path (Arch Linux,
                                              # etc.)
  set(LLVM_DIR "/usr/lib/llvm/lib/cmake/llvm")
  message(STATUS "Using LLVM from system installation at /usr/lib/llvm")

else()
  message(
    FATAL_ERROR "Could not find LLVM installation. Make sure LLVM is installed."
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
