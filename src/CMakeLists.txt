# Executable
add_executable(TransISA main.cpp)

# subdirectories
add_subdirectory(ir)
add_subdirectory(lexer)
add_subdirectory(llvm_ir)
add_subdirectory(optimizer)
add_subdirectory(parser)

# Link the libraries
target_link_libraries(TransISA 
    PRIVATE
    optimizer_lib
)