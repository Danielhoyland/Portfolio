add_library(GeometricTools INTERFACE ShaderDataTypes.h )
add_library(Framework::GeometricTools ALIAS GeometricTools)
target_include_directories(GeometricTools INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})

