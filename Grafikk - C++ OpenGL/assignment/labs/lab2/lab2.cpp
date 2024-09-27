#define GLFW_INCLUDE_NONE
#include "lab2.h"

#include <iostream>
#include <string>
#include <cmath>
#include <array>
#include <vector>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "GeometricTools.h"
#include "shaders/ChessShader.h"
#include "Vertex/VertexBuffer.h"
#include "Vertex/IndexBuffer.h"
#include "Vertex/VertexArray.h"
#include "Shader/Shader.h"

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

    return EXIT_SUCCESS;
}

unsigned Lab2::Run() {

    int greenColor1 = 0;
    int greenColor2 = 0;

    
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            colors[i][j] = false;
        }
    }

    colors[greenColor1][greenColor2] = true;

    // Create a vertex array
    auto vertexArray = std::make_shared<VertexArray>();

    // Generate grid vertices and topology
    constexpr int DivisionsXY = 8;
    auto gridVertices = GeometricTools::UnitGridGeometry2d(DivisionsXY, DivisionsXY);

    // Create a grid vertex buffer
    auto gridVertexBuffer = std::make_shared<VertexBuffer>(gridVertices.data(), gridVertices.size() * sizeof(float));
    BufferLayout gridBufferLayout = {
        {ShaderDataType::Float2, "position"},
        {ShaderDataType::Float2, "tcoords"}
    };
    gridVertexBuffer->SetLayout(gridBufferLayout);
    vertexArray->AddVertexBuffer(gridVertexBuffer);

    // Create an index buffer
    auto gridIndexBuffer = std::make_shared<IndexBuffer>(GeometricTools::UnitGridTopologyTriangles(DivisionsXY, DivisionsXY).data(), 6 * DivisionsXY * DivisionsXY);
    vertexArray->SetIndexBuffer(gridIndexBuffer);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 2, nullptr);
    glEnableVertexAttribArray(0);

    // Backgroundcolor
    glClearColor(0.5f, 0.0f, 0.0f, 1.0f);

    
    


    auto chessboardShader = std::make_shared<Shader>(chessVertexShaderSrc, chessFragmentShaderSrc);

    chessboardShader->Bind();
    //const std::string texturesDir = std::string(TEXTURES_DIR) + std::string("cube.jpeg");
    //GLuint texture = LoadTexture(texturesDir, 0);
    chessboardShader->UploadUniformInt("u_floorTextureSampler", 1);

    //Gets the greensquare uniform to change the one square that should be to green
    int positionLocation = chessboardShader->GetUniformLocation("position");

    glfwSetKeyCallback(window, input);	//sets callback
    glfwSwapInterval(1);				//vsync

    while (!glfwWindowShouldClose(window)) {
        glClear(GL_COLOR_BUFFER_BIT);
        

        if (keys[0] == 1 && greenColor1 != 0) {
            colors[greenColor1][greenColor2] = false;
            greenColor1--;
            colors[greenColor1][greenColor2] = true;
            keys[0] = 0;
        }
        else if (keys[0] == 1) {
            keys[0] = 0;
        }
        if (keys[1] == 1 && greenColor2 != 7) {
            colors[greenColor1][greenColor2] = false;
            greenColor2++;
            colors[greenColor1][greenColor2] = true;
            keys[1] = 0;
        }
        else if (keys[1] == 1) {
            keys[1] = 0;
        }
        if (keys[2] == 1 && greenColor2 != 0) {
            colors[greenColor1][greenColor2] = false;
            greenColor2--;
            colors[greenColor1][greenColor2] = true;
            keys[2] = 0;
        }
        else if (keys[2] == 1) {
            keys[2] = 0;
        }
        if (keys[3] == 1 && greenColor1 != 7) {
            colors[greenColor1][greenColor2] = false;
            greenColor1++;
            colors[greenColor1][greenColor2] = true;
            keys[3] = 0;
        }
        else if (keys[3] == 1) {
            keys[3] = 0;
        }
        //glm::vec2 selector = { 0.0f, 0.0f };
        //chessboardShader->UploadUniformFloat2("selector", selector); ??

        glUniform2i(positionLocation, greenColor1, greenColor2);

        // Poll for GLFW events
        glfwPollEvents();

       // Bind the vertex array
        vertexArray->Bind();

        // Draw the grid using glDrawElements
       
        glDrawElements(GL_TRIANGLES, gridVertices.size(), GL_UNSIGNED_INT, nullptr);
        //for (int i = 0; i < gridVertices.size(); i = i + 2) {
          //  glDrawArrays(GL_TRIANGLES, i, 3);
          //  glDrawArrays(GL_TRIANGLES, i + 1, 3);
        //}
        
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
    if (key == GLFW_KEY_LEFT && state == GLFW_RELEASE) { keys[0] = 1; }

    state = glfwGetKey(window, GLFW_KEY_UP);
    if (key == GLFW_KEY_UP && state == GLFW_RELEASE) { keys[1] = 1; }

    state = glfwGetKey(window, GLFW_KEY_DOWN);
    if (key == GLFW_KEY_DOWN && state == GLFW_RELEASE) { keys[2] = 1; }


    state = glfwGetKey(window, GLFW_KEY_RIGHT);
    if (key == GLFW_KEY_RIGHT && state == GLFW_RELEASE) { keys[3] = 1; }

    //selecting a square
    state = glfwGetKey(window, GLFW_KEY_ENTER);
    if (key == GLFW_KEY_ENTER && state == GLFW_PRESS) { keys[4] = 1; }

}
