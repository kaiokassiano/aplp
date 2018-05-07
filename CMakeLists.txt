cmake_minimum_required(VERSION 3.10)
project(aplp)

set(CMAKE_CXX_STANDARD 11)

add_executable(aplp
        game/cpp/main.cpp
        map/initial.xml)
