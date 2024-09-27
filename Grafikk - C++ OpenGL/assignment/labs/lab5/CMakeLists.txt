cmake_minimum_required(VERSION 3.15)

project(lab5)

add_executable(lab5 main.cpp 
					lab5.h
					lab5.cpp 
					shaders/ChessShader.h 
					shaders/CubeShader.h)   

target_link_libraries(lab5 PRIVATE glfw
								   glad 
								   glm
								   stb
								   Framework::GeometricTools	
								   Framework::GLFWApplication
								   Framework::Rendering)


add_custom_command(
  TARGET ${PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/cube.jpeg
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/cube.jpeg)

add_custom_command(
  TARGET ${PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/wood.jpeg
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/wood.jpeg)


target_compile_definitions(${PROJECT_NAME} PRIVATE
  TEXTURES_DIR="${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/")

target_compile_definitions(${PROJECT_NAME} PRIVATE STB_IMAGE_IMPLEMENTATION)

target_compile_definitions(${PROJECT_NAME} PRIVATE STB_IMAGE_STATIC)
