#ifndef LAB4_H
#define LAB4_H

#include "GLFWApplication.h"


#include "Texture/TextureManager.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <iostream>
#include <string>
#include <array>
#include <vector>


void input(GLFWwindow* window, int key, int scancode, int action, int mods);
GLuint LoadTexture(const std::string& filepath, GLuint slot);

class Lab4 : public GLFWApplication
{
public:
    Lab4(std::string name, std::string version);
    ~Lab4();

    unsigned Run();
    unsigned Init();

private:
    GLFWwindow* window;
    bool colors[8][8];            //what squares are which colors
    int present[8][8];            //what squares have a square within



    std::vector<float> gridVertices;
};

#endif