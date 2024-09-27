#define GLFW_INCLUDE_NONE
#include "lab3.h"

#include <iostream>
#include <string>
#include <cmath>
#include <array>
#include <vector>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "GeometricTools.h"
#include "shaders/CubeShader.h"
#include "Vertex/VertexBuffer.h"
#include "Vertex/IndexBuffer.h"
#include "Vertex/VertexArray.h"
#include "Shader/Shader.h"
#include "RenderCommands.h"

using namespace std;

int keys[5] = { 0, 0, 0, 0, 0};


Lab2::Lab2(std::string name, std::string version) : GLFWApplication(name, version)
{
    
    cout << "Created instance!" << endl;
    
}

Lab2::~Lab2()
{
    glfwDestroyWindow(window);
    cout << "Closed window!" << endl;
}

unsigned Lab2::Init() {
    // Initialization of glfw.
    if (!glfwInit())
    {
        std::cin.get();

        return EXIT_FAILURE;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    window = glfwCreateWindow(1000, 1000, "Hello World", nullptr, nullptr);
    if (window == nullptr)
    {
        glfwTerminate();

        return EXIT_FAILURE;
    }
    glfwMakeContextCurrent(window);

    //init GLaDOS
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        glfwTerminate();
        return EXIT_FAILURE;
    }
    glEnable(GL_DEPTH_TEST);


    return EXIT_SUCCESS;
}

unsigned Lab2::Run() {
    auto vertexArray = std::make_shared<VertexArray>();
    auto cubeVertices = GeometricTools::GenerateUnitCubeVertices();
    auto cubeVertexBuffer = std::make_shared<VertexBuffer>(cubeVertices.data(), cubeVertices.size() * sizeof(float));
    BufferLayout cubeBufferLayout = {
        { ShaderDataType::Float3, "a_Position" }
    };
    cubeVertexBuffer->SetLayout(cubeBufferLayout);
    vertexArray->AddVertexBuffer(cubeVertexBuffer);

    auto cubeIndices = GeometricTools::GenerateUnitCubeIndices();
    auto cubeIndexBuffer = std::make_shared<IndexBuffer>(cubeIndices.data(), cubeIndices.size());
    vertexArray->SetIndexBuffer(cubeIndexBuffer);

    // Make sure to enable the attribute
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(float) * 3, nullptr);
    glEnableVertexAttribArray(0);

    glm::vec3 rotationAxis(-1.0f, 0.0f, 0.0f);
 

    float angle = 45.0f;
    float aspect = 1000 / static_cast<float>(1000);  
    float near = 0.1f; 
    float far = 100.0f;  

    glm::mat4 projectionMatrix = glm::perspective(glm::radians(angle), aspect, near, far);

    glm::vec3 cameraPos = glm::vec3(0.0f, 0.0f, 5.0f);
    glm::vec3 cameraLooking = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 upDirection = glm::vec3(0.0f, 1.0f, 0.0f);

    glm::mat4 viewMatrix = glm::lookAt(cameraPos, cameraLooking, upDirection);

    glm::vec3 origo = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 scaleTransform = glm::vec3(1.0f, 1.0f, 1.0f);

    glm::mat4 modelMatrix = glm::mat4(1.0f);

    modelMatrix = glm::translate(modelMatrix, origo);
    modelMatrix = glm::rotate(modelMatrix, glm::radians(angle), rotationAxis); 
    modelMatrix = glm::scale(modelMatrix, scaleTransform);

    auto cubeShader = std::make_shared<Shader>(cubeVertexShaderSrc, cubeFragmentShaderSrc);
    cubeShader->Bind();


    cubeShader->SetMatrix4("u_projection2", projectionMatrix);
    cubeShader->SetMatrix4("u_view2", viewMatrix);
    cubeShader->SetMatrix4("u_model2", modelMatrix);

    int u_color = cubeShader->GetUniformLocation("u_Color");


    float rotationSpeed = 10.0f;
    glm::vec3 movx = glm::vec3(0.0f, 1.0f, 0.0f);
    glm::vec3 movy = glm::vec3(1.0f, 0.0f, 0.0f);


    // Backgroundcolor
    glm::vec4 backGround =glm::vec4(0.0f, 0.2f, 0.0f, 1.0f);
    RenderCommands::SetClearColor(backGround);

    glfwSetKeyCallback(window, input);	//sets callback
    glfwSwapInterval(1);				//vsync

    while (!glfwWindowShouldClose(window)) {
        RenderCommands::Clear();

        // Poll for GLFW events
        glfwPollEvents();

        // Handle key presses to rotate the cube
        if (keys[0] == 1) {
            // Rotate left (around the Y-axis)
            modelMatrix = glm::rotate(modelMatrix, glm::radians(-rotationSpeed), movx);
            keys[0] = 0;
        }

        if (keys[1] == 1) {
            // Rotate up (around the X-axis)
            modelMatrix = glm::rotate(modelMatrix, glm::radians(-rotationSpeed), movy);
            keys[1] = 0;
        }

        if (keys[2] == 1) {
            // Rotate down (around the X-axis)
            modelMatrix = glm::rotate(modelMatrix, glm::radians(rotationSpeed), movy);
            keys[2] = 0;
        }

        if (keys[3] == 1) {
            // Rotate right (around the Y-axis)
            modelMatrix = glm::rotate(modelMatrix, glm::radians(rotationSpeed), movx);
            keys[3] = 0;
        }
        cubeShader->SetMatrix4("u_model2", modelMatrix);

        // Set to filled mode
        RenderCommands::SetSolidMode();
        glUniform1f(u_color, 0.5f);
        vertexArray->Bind();
        // Draw the cube using glDrawElements
        RenderCommands::DrawIndex(vertexArray, GL_TRIANGLES);



        // Switch to wireframe mode
        RenderCommands::SetWireframeMode();
        glUniform1f(u_color, 0.0f);
        // Draw the cube again in wireframe mode
        RenderCommands::DrawIndex(vertexArray, GL_TRIANGLES);

        // Swap buffers
        glfwSwapBuffers(window);

        // Check for the escape key press to exit the loop
        if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS) {
            break;
        }
    }


    return EXIT_SUCCESS;
}



void input(GLFWwindow* window, int key, int scancode, int action, int mods) {
    int state;
    //exiting
    if (key == GLFW_KEY_Q) { glfwSetWindowShouldClose(window, 1); }		

    //movement
    state = glfwGetKey(window, GLFW_KEY_LEFT);
    if (key == GLFW_KEY_LEFT ) { keys[0] = 1; }

    state = glfwGetKey(window, GLFW_KEY_UP);
    if (key == GLFW_KEY_UP ) { keys[1] = 1; }

    state = glfwGetKey(window, GLFW_KEY_DOWN);
    if (key == GLFW_KEY_DOWN ) { keys[2] = 1; }


    state = glfwGetKey(window, GLFW_KEY_RIGHT);
    if (key == GLFW_KEY_RIGHT) { keys[3] = 1; }

    //selecting a square
    state = glfwGetKey(window, GLFW_KEY_ENTER);
    if (key == GLFW_KEY_ENTER && state == GLFW_PRESS) { keys[4] = 1; }

}
