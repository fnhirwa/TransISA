# library for code generation
add_library(codegen_lib Codegen.cpp)

# Include the headers
target_include_directories(codegen_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)

# link llvm_ir library
target_link_libraries(codegen_lib PRIVATE llvm_ir_lib ${LLVM_LIBS})
