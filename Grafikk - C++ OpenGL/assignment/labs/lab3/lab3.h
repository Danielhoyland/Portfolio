#ifndef LAB3_H
#define LAB3_H

#include "GLFWApplication.h"

#include <GLFW/glfw3.h>
#include <iostream>
#include <string>
#include <array>
#include <vector>


void input(GLFWwindow* window, int key, int scancode, int action, int mods);

class Lab2 : public GLFWApplication
{
public:
    Lab2(std::string name, std::string version);
    ~Lab2();

    unsigned Run();
    unsigned Init();


private:
    GLFWwindow* window;
    bool colors[8][8];            //what squares are which colors
    int present[8][8];            //what squares have a square within

    std::vector<float> gridVertices;
};

#endif