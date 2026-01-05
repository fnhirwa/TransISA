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

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Map LLVM components to library names
llvm_map_components_to_libnames(
  LLVM_LIBS
  support
  core
  mc
  x86asmparser
  x86codegen
  x86desc
  x86disassembler
  x86info
  aarch64asmparser
  aarch64codegen
  aarch64desc
  aarch64disassembler
  aarch64info
  aarch64utils)

# Add subdirectories for component libraries
add_subdirectory(Frontend)
add_subdirectory(Codegen)
add_subdirectory(Optimizer)
add_subdirectory(Utils)

# Executable
add_executable(TransISA main.cpp)

# Link the libraries
target_link_libraries(TransISA PRIVATE frontend_lib codegen_lib optimizer_lib
                                       utils_lib ${LLVM_LIBS})
