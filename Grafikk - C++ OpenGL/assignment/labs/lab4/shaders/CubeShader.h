#ifndef __CUBESHADER_H_
#define __CUBESHADER_H_
#include <string>

static const std::string cubeVertexShaderSrc = R"(

#version 430 core

layout(location = 0) in vec3 a_Position;

out vec4 vs_color; //this newly created variable that will output color to the fragment shader
                   // as opposed to the gl_Position placeholder, you can name it and type it
                   // as you see fit.
out vec3 vs_position;

uniform mat4 u_projection2;
uniform mat4 u_view2;
uniform mat4 u_model2;

void main()
{
    mat4 mvp = u_projection2 * u_view2 * u_model2;
    gl_Position = mvp * vec4(a_Position, 1.0);

    vs_position = a_Position;
}
)";

static const std::string cubeFragmentShaderSrc = R"(

#version 430 core

layout(binding = 1) uniform samplerCube uTexture;

in vec3 vs_position;

out vec4 finalColor;


void main()
{
    vec4 color = texture(uTexture, vs_position);
    // Semi-transparent color
    finalColor = vec4(color.rgb, 0.5);

}

)";

#endif // __CUBESHADER_H_
