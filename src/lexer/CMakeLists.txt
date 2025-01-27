# library for lexer
add_library(lexer_lib Lexer.cpp)

# Include the headers
target_include_directories(lexer_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)
