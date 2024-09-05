cmake_minimum_required(VERSION 3.25)
project(j VERSION 1.0)

set(CMAKE_CXX_STANDARD 23) # format, range
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS} -fsanitize=address -fsanitize=leak -fsanitize=undefined")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})

add_executable(j)
target_sources(j PRIVATE j.cpp)
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    target_compile_options(j PRIVATE "-fomit-frame-pointer")
    set_property(TARGET j PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

add_executable(random random.cpp)