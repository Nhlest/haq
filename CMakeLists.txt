cmake_minimum_required(VERSION 3.28)
project(haq)

add_definitions(-DGRAPHICS_API_OPENGL_43)
add_subdirectory(raylib)

set(CMAKE_CXX_STANDARD 23)

include_directories(raylib/src raygui/src)

link_libraries(raylib wayland-client GLEW)

add_executable(haq main.cpp parser.cpp)
