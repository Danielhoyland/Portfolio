#include <iostream>
// including GLFW libraries
#define GLFW_INCLUDE_NONE
#include <glad/glad.h>
#include <GLFW/glfw3.h>

class GLFWApplication
{
protected:
	GLFWwindow* window;
	std::string name;
	std::string version;
public:

	
	
	GLFWApplication(const std::string& name, const std::string& version);
	~GLFWApplication();

	// Initialization 
	virtual unsigned Init(); // Virtual function with default behavior.

	// Run function
	virtual unsigned Run() const = 0; // Pure virtual function that must be redefined.

	//...other functions...
};
