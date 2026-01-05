add_library(transisa_headers INTERFACE)

target_include_directories(
  transisa_headers INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                             $<INSTALL_INTERFACE:include>)

target_compile_features(transisa_headers INTERFACE cxx_std_17)
