#include "GLFWApplication.h"
#include <iostream>
#include <string>

using namespace std;

GLFWApplication::GLFWApplication(const string& name, const string& version) : name(name), version(version) {
    GLFWApplication::name = name; GLFWApplication::version = version;
}
GLFWApplication::~GLFWApplication() {
   
}
unsigned GLFWApplication::Init() {
    return 0;
}
unsigned GLFWApplication::Run() {
    return 0;
}
unsigned GLFWApplication::WindowSize(int x, int y) {
    windowX = x;
    windowY = y;
    return 0;
}


