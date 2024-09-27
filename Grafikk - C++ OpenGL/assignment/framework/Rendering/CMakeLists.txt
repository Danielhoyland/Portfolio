add_library(Rendering "Vertex/VertexBuffer.cpp" "Vertex/IndexBuffer.cpp" "Vertex/IndexBuffer.h" "Vertex/VertexArray.h" "Vertex/VertexArray.cpp" 
			"Vertex/BufferLayout.h" "Shader/ShaderDataTypes.h" "Shader/Shader.cpp" "Shader/Shader.h" 
			"RenderCommands.h" "Texture/TextureManager.h" "Texture/TextureManager.cpp" "Camera/Camera.h" "Camera/OrthographicCamera.h" 
			"Camera/PerspectiveCamera.h" "Camera/OrthographicCamera.cpp" "Camera/PerspectiveCamera.cpp")

add_library(Framework::Rendering ALIAS Rendering)
target_include_directories(Rendering PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(Rendering PUBLIC glad glfw TCLAP glm stb)