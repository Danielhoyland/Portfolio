#ifndef __TRIANGLE_H_
#define __TRIANGLE_H_

#include <string>

static const std::string triangleVertexShaderSrc = R"(

#version 430 core

layout(location = 0) in vec3 a_Position;
layout(location = 1) in vec3 a_Color;



out vec4 v_Color;

void main()
{
    gl_Position = vec4(a_Position, 1.0f);
    v_Color = vec4(a_Color, 1.0f);
}

)";

static const std::string triangleFragmentShaderSrc = R"(

#version 430 core

in vec4 v_Color;
out vec4 color;

void main()
{
color = v_Color;
}

)";

#endif // __TRIANGLE_H_
