cmake_minimum_required(VERSION 3.15)

project(lab2)

add_executable(lab2 main.cpp 
					lab2.h
					lab2.cpp "shaders/ChessShader.h")   # Generate an executable named HelloWorld from main.cpp [7]

target_link_libraries(lab2 PRIVATE glfw
								   glad 
								   glm  
								   Framework::GeometricTools	
								   Framework::GLFWApplication
								   Framework::Rendering)
