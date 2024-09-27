#ifndef LAB5_H
#define LAB5_H

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

class Lab5 : public GLFWApplication
{
public:
    Lab5(std::string name, std::string version);
    ~Lab5();

    unsigned Run();
    unsigned Init();

private:
    GLFWwindow* window;
    bool colors[8][8];            //what squares are which colors
    int present[8][8];            //what squares have a square within



    std::vector<float> gridVertices;
};

#endif