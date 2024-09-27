#ifndef GLFW_APPLICATION_H
#define GLFW_APPLICATION_H

#include <iostream>
#include <string>

class GLFWApplication {
public:
    GLFWApplication(const std::string& name, const std::string& version);
    ~GLFWApplication();
    virtual unsigned Init();
    virtual unsigned Run();
    virtual unsigned WindowSize(int x, int y);

protected:
    int windowX, windowY;
    std::string version, name;
};


#endif