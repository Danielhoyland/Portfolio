#include <iostream>
// including GLFW libraries
#define GLFW_INCLUDE_NONE
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "exam.h"


int main(int argc, char** argv)
{
    examApplication app("Exam", "1.0");

    app.Init();

    return app.Run();
}
