# library for optimizer and code generation
add_library(optimizer_lib Optimizer.cpp)

# Include the headers
target_include_directories(optimizer_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)

# link llvm_ir library
target_link_libraries(optimizer_lib PRIVATE llvm_ir_lib ${LLVM_LIBS})
