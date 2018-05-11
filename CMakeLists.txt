cmake_minimum_required(VERSION 3.10)
project(pakmen)

set(CMAKE_CXX_STANDARD 11)

find_package(Curses REQUIRED)
include_directories(${CURSES_INCLUDE_DIR})

add_executable(pakmen
  game/cpp/main.cpp)

target_link_libraries(pakmen ${CURSES_LIBRARIES})
