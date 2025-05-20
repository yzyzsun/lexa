cmake_minimum_required(VERSION 3.20)
project(lexa-llvm-passes)

find_package(LLVM CONFIG)

add_definitions(${LLVM_DEFINITIONS})
link_directories(${LLVM_LIBRARY_DIRS})
include_directories(${LLVM_INCLUDE_DIRS})
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

add_subdirectory(addNoCapture)
add_subdirectory(attachMetadata)
add_subdirectory(markInvariant)
add_subdirectory(generateStackMap)