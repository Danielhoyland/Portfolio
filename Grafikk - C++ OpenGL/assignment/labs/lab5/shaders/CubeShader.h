#ifndef __CUBESHADER_H_
#define __CUBESHADER_H_
#include <string>

static const std::string cubeVertexShaderSrc = R"(

#version 430 core

layout(location = 0) in vec3 a_Position;
layout(location = 1) in vec3 i_normal; 

out vec4 vs_normal;
out vec3 vs_position;
out vec4 vs_fragPosition;

uniform mat4 u_projection2;
uniform mat4 u_view2;
uniform mat4 u_model2;

void main()
{
    mat4 mvp = u_projection2 * u_view2 * u_model2;
    gl_Position = mvp * vec4(a_Position, 1.0);

    vs_normal = normalize(u_model2 * vec4(i_normal, 1.0));
    vs_position = a_Position;
    vs_fragPosition = u_model2 * vec4(a_Position, 1.0);
}
)";

static const std::string cubeFragmentShaderSrc = R"(

#version 430 core

layout(binding = 1) uniform samplerCube uTexture;

in vec3 vs_position;
in vec4 vs_fragPosition;
in vec4 vs_normal;

out vec4 finalColor;

uniform float u_ambientStrength = 1;
uniform vec3 u_lightSourcePosition;
uniform vec3 u_lightColor;
uniform vec3 u_cameraPosition;
uniform float u_specularStrength = 0.5f;
uniform vec3 u_specularColor; 

void main()
{
    vec4 color = texture(uTexture, vs_position);
    
    vec3 lightDirection = normalize(vec3(u_lightSourcePosition - vs_fragPosition.xyz));
    float diffuseStrength = max(dot(lightDirection, vs_normal.xyz), 0.0f);

    vec3 reflectedLight = normalize(reflect(-lightDirection, vs_normal.xyz));
    vec3 observerDirection = normalize(u_cameraPosition - vs_fragPosition.xyz);
    float specFactor = pow(max(dot(observerDirection, reflectedLight), 0.0), 12);
    float specular = specFactor * u_specularStrength;

    // Calculate the final color by combining ambient, diffuse, and specular reflections
    vec3 ambientDiffuseColor = color.rgb * (u_ambientStrength + diffuseStrength);
    vec3 specularColor = specular * u_specularColor; // Apply specular color
    finalColor = vec4((ambientDiffuseColor + specularColor) * u_lightColor, 1.0);
}


)";

#endif // __CUBESHADER_H_
