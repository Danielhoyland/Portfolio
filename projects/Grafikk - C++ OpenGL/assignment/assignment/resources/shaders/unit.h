#ifndef __UNIT_H_
#define __UNIT_H_
#include <string>

static const std::string unitVertexShaderSrc = R"(

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

static const std::string unitFragmentShaderSrc = R"(

#version 430 core

layout(binding = 1) uniform samplerCube uTexture;

in vec3 vs_position;
uniform bool u_textureBool = false;
uniform bool u_colorBool = false;
uniform bool u_green = false;
uniform bool u_selected = false;
out vec4 finalColor;


// Dark Red
vec3 darkRed = vec3(0.5, 0.0, 0.0);

// Dark Blue
vec3 darkBlue = vec3(0.0, 0.0, 0.5);

void main()
{   
    if(u_selected){
        finalColor = vec4(1, 1, 0, 1);
    }
    else if(u_green){
        finalColor = vec4(0.5, 0.8, 0.0, 1.0);
    }
    else {
        if(u_colorBool){
            finalColor = vec4(darkBlue,1.0);
        }else{
            finalColor = vec4(darkRed,1.0);
        }
    }
    if(u_textureBool){
    vec4 color = texture(uTexture, vs_position);
    // Semi-transparent color
    finalColor = mix(finalColor, color, 0.2);
    }
}

)";


#endif // __UNIT_H_
