file(GLOB PAKMEN_SOURCES *.cpp)
file(GLOB PAKMEN_HEADERS *.h)

list(APPEND PAKMEN_LINK_LIBRARIES ${CURSES_LIBRARIES})

add_executable(pakmen ${PAKMEN_SOURCES} ${PAKMEN_HEADERS} utils.h utils.cpp)

target_link_libraries(pakmen ${PAKMEN_LINK_LIBRARIES})

