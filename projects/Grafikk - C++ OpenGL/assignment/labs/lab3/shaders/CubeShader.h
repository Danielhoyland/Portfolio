#ifndef __CUBESHADER_H_
#define __CUBESHADER_H_
#include <string>

static const std::string cubeVertexShaderSrc = R"(

#version 430 core

layout(location = 0) in vec3 a_Position;

out vec4 vs_color; //this newly created variable that will output color to the fragment shader
                   // as opposed to the gl_Position placeholder, you can name it and type it
                   // as you see fit.
out vec3 pos;

uniform mat4 u_projection2;
uniform mat4 u_view2;
uniform mat4 u_model2;

void main()
{
    mat4 mvp = u_projection2 * u_view2 * u_model2;
    gl_Position = mvp * vec4(a_Position, 1.0);

    pos = a_Position;
}
)";

static const std::string cubeFragmentShaderSrc = R"(

#version 430 core

in vec3 pos;
uniform float u_Color; // Change 'vec1' to 'float' here
out vec4 color;


void main()
{

    float i = sin(pos.x) + 0.5f;
    color = vec4(u_Color, 0.0f, i, 1.0f);
}

)";

#endif // __CUBESHADER_H_
