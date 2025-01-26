# library for custom ir
add_library(ir_lib IRBuilder.cpp)

# Include the headers
target_include_directories(ir_lib PUBLIC ${CMAKE_SOURCE_DIR}/include)

# custom IR depends on parser output so we link the parser library
target_link_libraries(ir_lib PRIVATE parser_lib)