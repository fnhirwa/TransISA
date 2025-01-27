# library for parser
add_library(parser_lib Parser.cpp)

# Include the headers
target_include_directories(parser_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)

# Parse depends on lexer so we link the lexer library
target_link_libraries(parser_lib PRIVATE lexer_lib)
