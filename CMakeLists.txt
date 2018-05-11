cmake_minimum_required(VERSION 3.10)
project(aplp)

set(CMAKE_CXX_STANDARD 11)

find_package(Curses REQUIRED)
include_directories(${CURSES_INCLUDE_DIR})

add_executable(aplp
        game/cpp/main.cpp
        map/initial.xml)

target_link_libraries(aplp ${CURSES_LIBRARIES})
