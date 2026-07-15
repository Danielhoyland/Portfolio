#include <iostream>
#include <GLFWApplication.h>
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <string>

GLFWApplication::GLFWApplication(const std::string& name, const std::string& version) : name(name), version(version) {
    
    GLFWApplication::name = name; 
    GLFWApplication::version = version;

}

unsigned int GLFWApplication::Init() {
	
    if (!glfwInit()) {
		return -1;
	}
    /* Creating window */ // 640 480
    GLFWApplication::window = glfwCreateWindow(1024, 1024, name.data(), NULL, NULL);
    if (!GLFWApplication::window)
    {
        glfwTerminate();
        return -1;
    }
    /* Make the window's context current */
    glfwMakeContextCurrent(GLFWApplication::window);

    /* Setting glfw version i think*/
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);

    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        glfwTerminate();
        return EXIT_FAILURE;
    }

    glEnable(GL_DEPTH_TEST);
    // Enable blending
    glEnable(GL_BLEND);
    // Set the blending function: s*alpha + d(1-alpha)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    return 0;
}

GLFWApplication::~GLFWApplication()
{
    // program termination
    glfwDestroyWindow(GLFWApplication::window);
    glfwTerminate();
}
