cmake_minimum_required(VERSION 3.15)

project(lab1)

add_executable(lab1 main.cpp shaders/triangle.h
	shaders/square.h)   # Generate an executable named HelloWorld from main.cpp [7]

target_link_libraries(lab1 PRIVATE glfw)
target_link_libraries(lab1 PRIVATE glad)
target_link_libraries(lab1 PRIVATE TCLAP)
target_link_libraries(lab1 PRIVATE glm)
target_link_libraries(lab1 PRIVATE GeometricTools)