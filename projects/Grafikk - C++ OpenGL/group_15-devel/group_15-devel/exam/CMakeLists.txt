add_executable(exam 
	main.cpp 
	"exam.cpp"
 "shaders/exam_shader.h")   

target_link_libraries(exam PRIVATE glfw glad Framework::GLFWApplication Framework::GeometricTools Framework::Rendering Framework::Shaders)

add_custom_command(
  TARGET exam POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/walltexture.png
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/walltexture.png)

add_custom_command(
  TARGET exam POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/floortexture.jpg
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/floortexture.jpg)

add_custom_command(
  TARGET exam POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/resources/textures/boxtexture.png
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/boxtexture.png)

target_compile_definitions(exam PRIVATE
  TEXTURES_DIR="${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/resources/textures/")

target_compile_definitions(exam PRIVATE STB_IMAGE_IMPLEMENTATION)

