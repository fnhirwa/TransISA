# library for llvm_ir
add_library(llvm_ir_lib LLVMIRGenerator.cpp)

# Include the headers
target_include_directories(llvm_ir_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)

# we will generate LLVM IR from custom IR so we link the ir library
target_link_libraries(llvm_ir_lib PRIVATE parser_lib)

# Link LLVM libraries
target_link_libraries(llvm_ir_lib PRIVATE ${LLVM_LIBS})
