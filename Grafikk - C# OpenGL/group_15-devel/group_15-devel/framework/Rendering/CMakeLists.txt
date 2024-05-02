add_library(Rendering VertexBuffer.cpp IndexBuffer.cpp VertexArray.cpp "VertexBuffer.h" "IndexBuffer.h"  "BufferLayout.h"  "VertexArray.h"  "RenderCommands.h" "TextureManager.h" "TextureManager.cpp" "Camera/Camera.h" "Camera/OrthographicCamera.cpp" "Camera/PerspectiveCamera.cpp")
add_library(Framework::Rendering ALIAS Rendering)
target_include_directories(Rendering INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(Rendering PUBLIC glad glm stb Framework::GeometricTools)
