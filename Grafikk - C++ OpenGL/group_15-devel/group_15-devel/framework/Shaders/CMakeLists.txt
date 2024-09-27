add_library(Shaders Shaders.cpp Shaders.h)
add_library(Framework::Shaders ALIAS Shaders)
target_include_directories(Shaders INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(Shaders PUBLIC glad glm glfw Framework::GeometricTools)
