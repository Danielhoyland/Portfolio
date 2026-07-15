#define GLFW_INCLUDE_NONE
#include "lab4.h"

#include <iostream>
#include <string>
#include <cmath>
#include <array>
#include <vector>

#include <GLFW/glfw3.h>
#include "Texture/TextureManager.h"
#include "GeometricTools.h"
#include "shaders/ChessShader.h"
#include "shaders/CubeShader.h"
#include "Vertex/VertexBuffer.h"
#include "Vertex/IndexBuffer.h"
#include "Vertex/VertexArray.h"
#include "Shader/Shader.h"
#include "RenderCommands.h"
#include "Camera/OrthographicCamera.h"
#include "Camera/PerspectiveCamera.h"



using namespace std;

int keys[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0};


Lab4::Lab4(std::string name, std::string version) : GLFWApplication(name, version)
{
    
    cout << "Created instance!" << endl;
    
}

Lab4::~Lab4()
{
    glfwDestroyWindow(window);
    cout << "Closed window!" << endl;
}

unsigned Lab4::Init() {
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
    // Enable blending
    glEnable(GL_BLEND);
    // Set the blending function: s*alpha + d(1-alpha)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    return EXIT_SUCCESS;
}

unsigned Lab4::Run() {

    //CHESSBOARD
    int greenColor1 = 0;
    int greenColor2 = 0;


    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            colors[i][j] = false;
        }
    }

    colors[greenColor1][greenColor2] = true;

    auto vertexArray = std::make_shared<VertexArray>();
    constexpr int DivisionsXY = 8;
    auto gridVertices = GeometricTools::UnitGridGeometry2d(DivisionsXY, DivisionsXY);
    auto gridVertexBuffer = std::make_shared<VertexBuffer>(gridVertices.data(), gridVertices.size() * sizeof(float));
    BufferLayout gridBufferLayout = {
         {ShaderDataType::Float2, "position"},
         {ShaderDataType::Float2, "tcoords"}

    };
    gridVertexBuffer->SetLayout(gridBufferLayout);
    vertexArray->AddVertexBuffer(gridVertexBuffer);
    auto gridIndexBuffer = std::make_shared<IndexBuffer>(GeometricTools::UnitGridTopologyTriangles(DivisionsXY, DivisionsXY).data(), 6 * DivisionsXY * DivisionsXY);
    vertexArray->SetIndexBuffer(gridIndexBuffer);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 2, nullptr);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 2, nullptr);
    glEnableVertexAttribArray(1);

    TextureManager* textureManager = TextureManager::GetInstance();
    // Chessboard variables
    glm::vec3 chessboardRotationAxis(-1.0f, 0.0f, 0.0f);
    
    float chessboardAngle = 45.0f;
    float chessboardAspect = 1.0f;  // Assuming square aspect ratio
    float chessboardNear = 0.1f;
    float chessboardFar = 10.0f;
    
    glm::vec3 chessboardCameraPos = glm::vec3(0.0f, 0.0f, 5.0f);
    glm::vec3 chessboardCameraLooking = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 chessboardUpDirection = glm::vec3(0.0f, 1.0f, 0.0f);    
    
    PerspectiveCamera perspCamera(
        { chessboardAngle, chessboardAspect, chessboardAspect, chessboardNear, chessboardFar },
        chessboardCameraPos,
        chessboardCameraLooking,
        chessboardUpDirection
    );
    
    // Projection matrix for the chessboard
    glm::mat4 chessboardProjectionMatrix = perspCamera.GetProjectionMatrix();

    // View matrix for the chessboard
    glm::mat4 chessboardViewMatrix = perspCamera.GetViewMatrix();

    glm::vec3 chessboardOrigo = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 chessboardScaleTransform = glm::vec3(2.0f, 2.0f, 2.0f);

    // Model matrix for the chessboard
    glm::mat4 chessboardModelMatrix = glm::mat4(1.0f);
    chessboardModelMatrix = glm::translate(chessboardModelMatrix, chessboardOrigo);
    chessboardModelMatrix = glm::rotate(chessboardModelMatrix, glm::radians(chessboardAngle), chessboardRotationAxis);
    chessboardModelMatrix = glm::scale(chessboardModelMatrix, chessboardScaleTransform);


    auto chessboardShader = std::make_shared<Shader>(chessVertexShaderSrc, chessFragmentShaderSrc);
    chessboardShader->Bind();
    
    chessboardShader->SetMatrix4("u_projection", chessboardProjectionMatrix);
    chessboardShader->SetMatrix4("u_view", chessboardViewMatrix);
    chessboardShader->SetMatrix4("u_model", chessboardModelMatrix);

    int positionLocation = chessboardShader->GetUniformLocation("position");

    const std::string texturesDir = std::string(TEXTURES_DIR) + std::string("wood.jpeg");
    textureManager->LoadTexture2DRGBA("chessTexture", texturesDir, 0);
    chessboardShader->Bind();
    chessboardShader->UploadUniformInt("uTexture", 0);


    //CUBE
    auto vertexArrayC = std::make_shared<VertexArray>();
    auto cubeVertices = GeometricTools::GenerateUnitCubeVertices();
    auto cubeVertexBuffer = std::make_shared<VertexBuffer>(cubeVertices.data(), cubeVertices.size() * sizeof(float));
    BufferLayout cubeBufferLayout = {
        { ShaderDataType::Float3, "a_Position" }
    };
    cubeVertexBuffer->SetLayout(cubeBufferLayout);
    vertexArrayC->AddVertexBuffer(cubeVertexBuffer);

    auto cubeIndices = GeometricTools::GenerateUnitCubeIndices();
    auto cubeIndexBuffer = std::make_shared<IndexBuffer>(cubeIndices.data(), cubeIndices.size());
    vertexArrayC->SetIndexBuffer(cubeIndexBuffer);

    // Make sure to enable the attribute
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(float) * 3, nullptr);
    glEnableVertexAttribArray(0);



    glm::vec3 rotationAxisC(-1.0f, 0.0f, 0.0f);
    float angleC = 45.0f;
    float aspectC = 1000 / static_cast<float>(1000);
    float nearC = 0.1f;
    float farC = 100.0f;



    glm::vec3 cameraPosC = glm::vec3(0.0f, 0.0f, 5.0f);
    glm::vec3 cameraLookingC = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 upDirectionC = glm::vec3(0.0f, 1.0f, 0.0f);

    PerspectiveCamera cubeCamera(
        { angleC, aspectC, aspectC, nearC, farC },
        cameraPosC,
        cameraLookingC,
        upDirectionC
    );

    glm::mat4 projectionMatrixC = cubeCamera.GetProjectionMatrix();

    glm::mat4 viewMatrixC = cubeCamera.GetViewMatrix();

    glm::vec3 origoC = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 scaleTransformC = glm::vec3(1.0f, 1.0f, 1.0f);

    glm::mat4 modelMatrixC = glm::mat4(1.0f);

    modelMatrixC = glm::translate(modelMatrixC, origoC);
    modelMatrixC = glm::rotate(modelMatrixC, glm::radians(angleC), rotationAxisC);
    modelMatrixC = glm::scale(modelMatrixC, scaleTransformC);

    auto cubeShader = std::make_shared<Shader>(cubeVertexShaderSrc, cubeFragmentShaderSrc);
    cubeShader->Bind();


    cubeShader->SetMatrix4("u_projection2", projectionMatrixC);
    cubeShader->SetMatrix4("u_view2", viewMatrixC);
    cubeShader->SetMatrix4("u_model2", modelMatrixC);
    const std::string texturesDirC = std::string(TEXTURES_DIR) + std::string("cube.jpeg");
    textureManager->LoadCubeMapRGBA("cubeTexture", texturesDirC, 0);
    cubeShader->UploadUniformInt("uTexture", 0); 

    float rotationSpeed = 5.0f;
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
        // Handle key presses to rotate the cube
        if (keys[4] == 1) {
            // Rotate left (around the Y-axis)
            modelMatrixC = glm::rotate(modelMatrixC, glm::radians(-rotationSpeed), movx);
            keys[4] = 0;
        }

        if (keys[5] == 1) {
            // Rotate up (around the X-axis)
            modelMatrixC = glm::rotate(modelMatrixC, glm::radians(-rotationSpeed), movy);
            keys[5] = 0;
        }

        if (keys[6] == 1) {
            // Rotate down (around the X-axis)
            modelMatrixC = glm::rotate(modelMatrixC, glm::radians(rotationSpeed), movy);
            keys[6] = 0;
        }

        if (keys[7] == 1) {
            // Rotate right (around the Y-axis)
            modelMatrixC = glm::rotate(modelMatrixC, glm::radians(rotationSpeed), movx);
            keys[7] = 0;
        }
        chessboardShader->Bind();
        glUniform2i(positionLocation, greenColor1, greenColor2);
        vertexArray->Bind();

        // Set to solid mode for the chessboard
        RenderCommands::SetSolidMode();
        glDrawElements(GL_TRIANGLES, gridVertices.size(), GL_UNSIGNED_INT, nullptr);

        chessboardShader->Unbind();
        vertexArrayC->Bind();
        cubeShader->Bind();
        cubeShader->SetMatrix4("u_model2", modelMatrixC);

        // Set to wireframe mode for the cube
        RenderCommands::SetSolidMode();
        RenderCommands::DrawIndex(vertexArrayC, GL_TRIANGLES);
        RenderCommands::SetWireframeMode();
        RenderCommands::DrawIndex(vertexArrayC, GL_TRIANGLES);

        cubeShader->Unbind();
        

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

    //chess movement
    state = glfwGetKey(window, GLFW_KEY_LEFT);
    if (key == GLFW_KEY_LEFT && state == GLFW_RELEASE) { keys[0] = 1; }

    state = glfwGetKey(window, GLFW_KEY_UP);
    if (key == GLFW_KEY_UP && state == GLFW_RELEASE) { keys[1] = 1; }

    state = glfwGetKey(window, GLFW_KEY_DOWN);
    if (key == GLFW_KEY_DOWN && state == GLFW_RELEASE) { keys[2] = 1; }


    state = glfwGetKey(window, GLFW_KEY_RIGHT);
    if (key == GLFW_KEY_RIGHT && state == GLFW_RELEASE) { keys[3] = 1; }


    //cube
    state = glfwGetKey(window, GLFW_KEY_A);
    if (key == GLFW_KEY_A) { keys[4] = 1; }

    state = glfwGetKey(window, GLFW_KEY_W);
    if (key == GLFW_KEY_W) { keys[5] = 1; }

    state = glfwGetKey(window, GLFW_KEY_S);
    if (key == GLFW_KEY_S ) { keys[6] = 1; }

    state = glfwGetKey(window, GLFW_KEY_D);
    if (key == GLFW_KEY_D) { keys[7] = 1; }

    //selecting a square
    state = glfwGetKey(window, GLFW_KEY_ENTER);
    if (key == GLFW_KEY_ENTER && state == GLFW_PRESS) { keys[8] = 1; }

}
