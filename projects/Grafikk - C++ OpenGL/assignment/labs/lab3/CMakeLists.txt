cmake_minimum_required(VERSION 3.15)

project(lab3)

add_executable(lab3 main.cpp 
					lab3.h
					lab3.cpp 
					shaders/ChessShader.h 
					shaders/CubeShader.h)   # Generate an executable named HelloWorld from main.cpp [7]

target_link_libraries(lab3 PRIVATE glfw
								   glad 
								   glm  
								   Framework::GeometricTools	
								   Framework::GLFWApplication
								   Framework::Rendering)
