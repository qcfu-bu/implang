cmake_minimum_required(VERSION 3.15)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_FLAGS_INIT "-fsanitize=undefined")
set(CMAKE_CXX_FLAGS_INIT "-fsanitize=undefined")

project(ImpLang LANGUAGES C CXX)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

add_executable(implang main.cpp)
set_property(TARGET implang PROPERTY CXX_STANDARD 17)

add_subdirectory(parsing)
add_subdirectory(language)
add_subdirectory(codegen)

llvm_config(implang USE_SHARED all)
