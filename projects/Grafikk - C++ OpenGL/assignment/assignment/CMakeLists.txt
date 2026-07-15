cmake_minimum_required(VERSION 3.15)

project(assignment)

add_executable(assignment main.cpp 
					assignment.h
					assignment.cpp  "resources/shaders/chess.h"  "resources/shaders/unit.h")   

target_link_libraries(assignment PRIVATE glfw
								   glad 
								   glm
								   stb
								   Framework::GeometricTools	
								   Framework::GLFWApplication
								   Framework::Rendering)


add_custom_command(
  TARGET ${PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/floor_texture.png
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/floor_texture.png)
  
  add_custom_command(
  TARGET ${PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/cube_texture.png
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/cube_texture.png)


target_compile_definitions(${PROJECT_NAME} PRIVATE
  TEXTURES_DIR="${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/")

target_compile_definitions(${PROJECT_NAME} PRIVATE STB_IMAGE_IMPLEMENTATION)

target_compile_definitions(${PROJECT_NAME} PRIVATE STB_IMAGE_STATIC)