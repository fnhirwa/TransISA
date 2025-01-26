# library for llvm_ir
add_library(llvm_ir_lib LLVMIRGenerator.cpp)

# Include the headers
target_include_directories(llvm_ir_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)

# we will generate LLVM IR from custom IR so we link the ir library
target_link_libraries(llvm_ir_lib PRIVATE ir_lib)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Map LLVM components to library names
llvm_map_components_to_libnames(LLVM_LIBS support core irreader mc)

# Link LLVM libraries
target_link_libraries(llvm_ir_lib
    PRIVATE
    ${LLVM_LIBS}
)