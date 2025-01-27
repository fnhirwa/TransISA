# include directories
target_include_directories(${PROJECT_NAME}
                           PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(lexer_lib PUBLIC include/lexer)
target_include_directories(parser_lib PUBLIC include/parser)
target_include_directories(ir_lib PUBLIC include/ir)
target_include_directories(llvm_ir_lib PUBLIC include/llvm_ir)
target_include_directories(optimizer_lib PUBLIC include/optimizer)
