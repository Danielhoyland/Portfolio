#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#include <glad/glad.h>
#include <iostream>
#include <tclap/CmdLine.h>
#include "shaders/triangle.h"
#include "shaders/square.h"

// -----------------------------------------------------------------------------
//  CREATE TRIANGLE
// -----------------------------------------------------------------------------
GLuint CreateTriangle()
{
    // Triangle vertices with interleaved position and color data
    GLfloat triangle[3 * 3 * 2] =
    {
      -0.5f, -0.5f, 0.0f, 1.0f, 1.0f, 0.0f,
      0.5f, -0.5f, 0.0f, 0.0f, 1.0f, 0.0f,
      0.0f, 0.5f, 0.0f, 0.0f, 1.0f, 1.0f
    };

    GLuint triangleVBO; // Vertex Buffer Object
    GLuint triangleVAO; // Vertex Array Object

    // Creating a Vertex Array Object (VAO). In OpenGL 4.5 and newer, glCreateVertexArrays can be used as an alternative for improved safety.
    glGenVertexArrays(1, &triangleVAO);
    glBindVertexArray(triangleVAO);

    // Generate and bind VBO
    glGenBuffers(1, &triangleVBO);
    glBindBuffer(GL_ARRAY_BUFFER, triangleVBO);

    glBufferData(GL_ARRAY_BUFFER, sizeof(triangle), triangle, GL_STATIC_DRAW);

    // Vertex positions
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);

    // Vertex colors
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);

    // Clean up
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    glDeleteBuffers(1, &triangleVBO);

    return triangleVAO;
}

// -----------------------------------------------------------------------------
//  CREATE SQUARE
// -----------------------------------------------------------------------------
GLuint CreateSquare()
{
    // Square vertices with interleaved position and color data
    GLfloat squareVertices[4 * 3 * 2] =
    {
      -0.5f, -0.5f, 0.0f, 1.0f, 0.0f, 0.0f,
       0.5f, -0.5f, 0.0f, 0.0f, 1.0f, 0.0f,
       0.5f, 0.5f, 0.0f, 0.0f, 0.0f, 1.0f,
      -0.5f, 0.5f, 0.0f, 1.0f, 1.0f, 0.0f
    };

    GLuint squareIndices[2 * 3] =
    {
      0, 1, 2,
      2, 3, 0
    };

    GLuint squareVBO; // Vertex Buffer Object
    GLuint squareEBO; // Element Buffer Object
    GLuint squareVAO; // Vertex Array Object

    // Creating a Vertex Array Object (VAO). In OpenGL 4.5 and newer, glCreateVertexArrays can be used as an alternative for improved safety.
    glGenVertexArrays(1, &squareVAO);
    glBindVertexArray(squareVAO);

    // Generate and bind VBO
    glGenBuffers(1, &squareVBO);
    glBindBuffer(GL_ARRAY_BUFFER, squareVBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(squareVertices), squareVertices, GL_STATIC_DRAW);

    // Generate and bind EBO
    glGenBuffers(1, &squareEBO);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, squareEBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(squareIndices), squareIndices, GL_STATIC_DRAW);

    // Vertex positions
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);

    // Vertex colors
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);

    // Clean up
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    glDeleteBuffers(1, &squareVBO);

    return squareVAO;
}

void CleanVAO(GLuint& vao)
{
    glBindVertexArray(vao);
    GLint maxVertexAttribs;
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &maxVertexAttribs);
    for (GLint i = 0; i < maxVertexAttribs; i++)
    {
        glDisableVertexAttribArray(i);
    }
    glBindVertexArray(0);
    glDeleteVertexArrays(1, &vao);
    vao = 0;
}
void GLSquareDraw() {
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, (const void*)0);
}
void GLTriangleDraw() {
    glDrawArrays(GL_TRIANGLES, 0, 3);
}

void shadeComp(std::string vs, std::string fs) {
    // Compile the vertex shader
    auto vertexShader = glCreateShader(GL_VERTEX_SHADER);
    const GLchar* vss = vs.c_str();
    glShaderSource(vertexShader, 1, &vss, nullptr);
    glCompileShader(vertexShader);

    // Compile the fragment shader
    auto fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    const GLchar* fss = fs.c_str();
    glShaderSource(fragmentShader, 1, &fss, nullptr);
    glCompileShader(fragmentShader);

    // Create a shader program
    auto shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);

    glLinkProgram(shaderProgram);

    // Shader objects can be deleted once they 
    // have been linked in a shader program
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    glUseProgram(shaderProgram);
}
GLuint CompileShader(const std::string& vertexShaderSrc,
    const std::string& fragmentShaderSrc)
{
    // Convert shader source code from std::string to raw char pointer.
    auto vertexSrc = vertexShaderSrc.c_str();
    auto fragmentSrc = fragmentShaderSrc.c_str();

    // Compile vertex shader
    auto vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexSrc, nullptr);
    glCompileShader(vertexShader);

    // Compile fragment shader
    auto fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentSrc, nullptr);
    glCompileShader(fragmentShader);

    // Link shaders into a program
    auto shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);

    // Clean up shader objects as they're no longer needed after linking
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    return shaderProgram;
}

void GLFWErrorCallback(int code, const char* description)
{
    std::cerr << "Error " << "0x" << std::hex << code << ':' << description << "\n";
}

int main(int argc, char** argv)
{
    bool squ, tri;
    float deltaTime = 0, lastFrame = 0, currentFrame = 0, toatalTime = 0, lastFrameRun = 0;
    // Initialization of glfw.
    if (!glfwInit())
    {
        std::cin.get();

        return EXIT_FAILURE;
    }

    glfwWindowHint(GLFW_RESIZABLE, false);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    auto window = glfwCreateWindow(1000, 1000, "Hello Pain", nullptr, nullptr);
    if (window == nullptr)
    {
        glfwTerminate();

        return EXIT_FAILURE;
    }
    glfwMakeContextCurrent(window);

    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        glfwTerminate();
        return EXIT_FAILURE;
    }


    // saving square data in form and color
    auto squareVAO = CreateSquare();
    auto squareShaderProgram = CompileShader(squareVertexShaderSrc,
        squareFragmentShaderSrc);
    // saving triangle data in form and color
    auto triangleVAO = CreateTriangle();
    auto triangleShaderProgram = CompileShader(triangleVertexShaderSrc,
        triangleFragmentShaderSrc);
    

    // Backgroundcolor
    glClearColor(0.5f, 0.0f, 0.0f, 1.0f);


    bool alternate = false;
    while (!glfwWindowShouldClose(window))
    {
        currentFrame = glfwGetTime();

        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;
        toatalTime += deltaTime;

        if (currentFrame - lastFrame > 1.0)
        {
            alternate = !alternate;
        }

        glClear(GL_COLOR_BUFFER_BIT);

        auto greenValue = (tan(toatalTime) / 2.0f) + 0.5f;
        auto redValue = (sin(toatalTime) / 2.0f) + 0.5f;
        auto blueValue = (cos(toatalTime) / 2.0f) + 0.5f;
        auto change = (sin(toatalTime) / 2.0f);
        auto brownValueR = 0.37f + 0.37f * change;
        auto brownValueG = 0.22f + 0.22f * change;
        auto brownValueB = 0.12f + 0.12f * change;

        glClearColor(0.5f+change, 0.0f, 0.0f, 1.0f);
        
        auto vertexColorLocation = glGetUniformLocation(squareShaderProgram, "u_Color");
        glUseProgram(squareShaderProgram);
        glBindVertexArray(squareVAO);
        glUniform4f(vertexColorLocation, brownValueR, brownValueG, brownValueB, 1.0f);
        GLSquareDraw();

        
        glUseProgram(triangleShaderProgram);
        glBindVertexArray(triangleVAO);
        GLTriangleDraw();
       
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
     
    // Cleanup of used storage
    CleanVAO(triangleVAO);
    CleanVAO(squareVAO);
    glfwTerminate();



    return EXIT_SUCCESS;
}
