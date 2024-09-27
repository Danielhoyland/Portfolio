#ifndef __CHESSSHADER_H_
#define __CHESSSHADER_H_

#include <string>

static const std::string chessVertexShaderSrc = R"(

#version 430 core

layout(location = 0) in vec2 a_Position;
layout(location = 1) in vec2 tcoords;
out vec2 vs_tcoords; 
out vec4 vs_color; //this newly created variable that will output color to the fragment shader
                   // as opposed to the gl_Position placeholder, you can name it and type it
                   // as you see fit.
out vec2 pos;

uniform mat4 u_projection;
uniform mat4 u_view;
uniform mat4 u_model;

void main()
{

mat4 mvp = u_projection * u_view * u_model;

gl_Position = mvp * vec4(a_Position, 0.0, 1.0);

pos = a_Position;

vs_tcoords = tcoords;
}

)";

static const std::string chessFragmentShaderSrc = R"(

#version 430 core

#define M_PI 3.14

in vec2 pos;
in vec2 vs_tcoords; 


layout(binding = 0) uniform sampler2D uTexture; 

uniform ivec2 position;
uniform float u_ambientStrength = 1;
out vec4 Color; 

void main()
{
    if ((pos.x >= (1.0 / 8.0 * float(position.x) - 0.5)) && (pos.x <= (1.0 / 8.0 * float(position.x) - 0.5 + 1.0 / 8.0)) && (pos.y >= (1.0 / 8.0 * float(position.y) - 0.5)) && (pos.y <= (1.0 / 8.0 * float(position.y) - 0.5 + 1.0 / 8.0)))
    {
        Color = vec4(0, 1, 0, 1); // Set the color when the position matches
    }
    else
    {
        if (sin(pos.x * M_PI * 2 * 4) < 0)
        {
            if (sin(pos.y * M_PI * 2 * 4) < 0)
            {
                Color = vec4(1, 0.9, 0.9, 1.0);
            }
            else
            {
                Color = vec4(0, 0, 0.2, 1.0);
            }
        }
        else
        {
            if (sin(pos.y * M_PI * 2 * 4) > 0)
            {
                Color = vec4(1, 0.9, 0.9, 1.0);
            }
            else
            {
                Color = vec4(0, 0, 0.2, 1.0);
            }
        }
    }
   
    Color = mix(Color, texture(uTexture, vs_tcoords), 0.7);
    Color = u_ambientStrength * Color;

}


)";

#endif // __CHESSSHADER_H_
